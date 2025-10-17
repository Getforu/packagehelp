# Copyright (C) 2025 packagehelp Team
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Classify installation error and provide suggestions
#' @param error_msg error message
#' @param pkg_name package name
#' @param recommended_version recommended version (optional)
#' @return list with error type and suggestion
#' @keywords internal
classify_install_error <- function(error_msg, pkg_name = "", recommended_version = NULL) {
  error_msg <- tolower(as.character(error_msg))

  # Define large/problematic packages that benefit from remotes installation
  large_problematic_packages <- c("caret", "rmda", "xgboost", "arrow", "rJava", "sf", "terra", "keras", "tensorflow")

  if (grepl("timeout|timed out|cannot open url|download.*failed|failed to connect|connection.*failed", error_msg)) {
    suggestion <- "检查网络连接，或更换CRAN镜像后重试"
    if (!is.null(recommended_version) && pkg_name %in% large_problematic_packages) {
      suggestion <- sprintf("检查网络连接后，尝试手动安装（请运行下方代码）：\n      remotes::install_version(\"%s\", version = \"%s\", dependencies = TRUE)", pkg_name, recommended_version)
    }
    return(list(
      type = "network",
      type_cn = "网络问题",
      suggestion = suggestion,
      detail = "无法连接到软件包服务器或下载超时",
      manual_command = if (!is.null(recommended_version) && pkg_name %in% large_problematic_packages)
        sprintf("remotes::install_version(\"%s\", version = \"%s\", dependencies = TRUE)", pkg_name, recommended_version) else NULL
    ))
  } else if (grepl("depends on.*but|dependency.*not available|package.*required.*not available|version.*required", error_msg)) {
    return(list(
      type = "dependency",
      type_cn = "依赖冲突",
      suggestion = "先更新相关依赖包，或手动安装缺失的依赖",
      detail = "所需的依赖包版本不匹配或缺失"
    ))
  } else if (grepl("compilation failed|non-zero exit status|error.*compiling|make.*error|gcc|g\\+\\+", error_msg)) {
    os_type <- Sys.info()["sysname"]
    if (os_type == "Windows") {
      tool_suggestion <- "安装 Rtools (https://cran.r-project.org/bin/windows/Rtools/)"
    } else if (os_type == "Darwin") {
      tool_suggestion <- "安装 Xcode Command Line Tools: xcode-select --install"
    } else {
      tool_suggestion <- "安装编译工具: sudo apt-get install r-base-dev (Ubuntu/Debian)"
    }
    return(list(
      type = "compilation",
      type_cn = "编译失败",
      suggestion = tool_suggestion,
      detail = "源码编译过程失败，可能缺少编译工具"
    ))
  } else if (grepl("permission denied|cannot create|cannot remove|cannot open file.*permission", error_msg)) {
    return(list(
      type = "permission",
      type_cn = "权限不足",
      suggestion = "检查安装目录的读写权限，或以管理员权限运行R",
      detail = "没有足够的权限写入安装目录"
    ))
  } else if (grepl("disk|space|cannot write|no space left", error_msg)) {
    return(list(
      type = "disk_space",
      type_cn = "磁盘空间",
      suggestion = "清理磁盘空间后重试",
      detail = "磁盘空间不足"
    ))
  } else if (grepl("package.*is not available|not available for", error_msg)) {
    return(list(
      type = "not_available",
      type_cn = "包不可用",
      suggestion = sprintf("该版本可能已下架，尝试安装最新版本或检查包名是否正确"),
      detail = "CRAN上找不到指定版本的包"
    ))
  } else {
    suggestion <- "查看详细错误信息，或咨询技术支持"
    if (!is.null(recommended_version) && pkg_name %in% large_problematic_packages) {
      suggestion <- sprintf("尝试手动安装（请运行下方代码）：\n      remotes::install_version(\"%s\", version = \"%s\", dependencies = TRUE)\n      或咨询技术支持", pkg_name, recommended_version)
    }
    return(list(
      type = "unknown",
      type_cn = "未知错误",
      suggestion = suggestion,
      detail = error_msg,
      manual_command = if (!is.null(recommended_version) && pkg_name %in% large_problematic_packages)
        sprintf("remotes::install_version(\"%s\", version = \"%s\", dependencies = TRUE)", pkg_name, recommended_version) else NULL
    ))
  }
}

#' Install package with timeout and retry mechanism
#' @param pkg_name package name
#' @param recommended_version recommended version
#' @param pkg_type package type (binary/source/both)
#' @param max_retries maximum retry attempts
#' @param timeout timeout in seconds
#' @return list with installation result
#' @keywords internal
install_package_with_retry <- function(pkg_name, recommended_version, pkg_type = "binary",
                                       max_retries = 3, timeout = NULL) {

  # Determine timeout based on package size estimation
  if (is.null(timeout)) {
    # Large packages that typically need more time
    large_packages <- c("arrow", "xgboost", "rJava", "sf", "rgdal", "terra", "keras", "tensorflow")
    # Medium packages
    medium_packages <- c("ggplot2", "dplyr", "tidyr", "rms", "caret", "shiny", "plotly", "DynNom")

    if (pkg_name %in% large_packages) {
      timeout <- 600  # 10 minutes for large packages
      max_retries <- 3
    } else if (pkg_name %in% medium_packages) {
      timeout <- 300  # 5 minutes for medium packages
      max_retries <- 3
    } else {
      timeout <- 180  # 3 minutes for small packages
      max_retries <- 2
    }
  }

  # Save and set timeout option
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  last_error <- NULL

  for (attempt in 1:max_retries) {
    install_result <- tryCatch({
      # Try installation
      install.packages(pkg_name,
                      dependencies = TRUE,
                      type = pkg_type,
                      quiet = TRUE)

      # Verify installation
      if (requireNamespace(pkg_name, quietly = TRUE)) {
        current_version <- as.character(packageVersion(pkg_name))

        # Try to get exact version if available and current version is lower
        if (compareVersion(current_version, recommended_version) < 0) {
          if (requireNamespace("remotes", quietly = TRUE)) {
            tryCatch({
              remotes::install_version(pkg_name, version = recommended_version,
                                     dependencies = TRUE, upgrade = "never",
                                     type = pkg_type, quiet = TRUE)
            }, error = function(e) {
              # Keep successfully installed version even if exact match fails
            })
          }
        }

        return(list(
          success = TRUE,
          attempt = attempt,
          version = as.character(packageVersion(pkg_name)),
          error = NULL
        ))
      } else {
        stop("Package installation succeeded but cannot be loaded")
      }
    }, error = function(e) {
      last_error <<- e$message
      NULL
    })

    # If successful, return immediately
    if (!is.null(install_result) && install_result$success) {
      return(install_result)
    }

    # If this is not the last attempt, wait before retry
    if (attempt < max_retries) {
      wait_times <- c(2, 5, 10)
      wait_time <- wait_times[min(attempt, length(wait_times))]
      Sys.sleep(wait_time)
    }
  }

  # All retries failed - try source installation as last resort
  if (pkg_type != "source") {
    final_attempt <- tryCatch({
      install.packages(pkg_name,
                      dependencies = TRUE,
                      type = "source",
                      quiet = TRUE)

      if (requireNamespace(pkg_name, quietly = TRUE)) {
        return(list(
          success = TRUE,
          attempt = max_retries + 1,
          version = as.character(packageVersion(pkg_name)),
          error = NULL,
          note = "installed_from_source"
        ))
      }
      NULL
    }, error = function(e) {
      last_error <<- e$message
      NULL
    })

    if (!is.null(final_attempt) && final_attempt$success) {
      return(final_attempt)
    }
  }

  # Complete failure
  return(list(
    success = FALSE,
    attempt = max_retries,
    version = NULL,
    error = last_error
  ))
}

#' Analyze package status
#' @param package_defs definitions
#' @return analysis
#' @keywords internal
analyze_package_status <- function(package_defs) {
  r_version <- getRversion()
  version_ok <- compareVersion(as.character(r_version), package_defs$min_r_version) >= 0

  base_missing <- c()
  for (pkg in package_defs$base_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      base_missing <- c(base_missing, pkg)
    }
  }

  if (!version_ok) {
    cat(sprintf("警告：R版本过低 (当前: %s, 需要: %s+)\n", r_version, package_defs$min_r_version))
  }

  if (length(base_missing) == 0) {
    cat("基础包：已全部安装\n")
  } else {
    cat(sprintf("基础包：缺失 %d 个\n", length(base_missing)))
  }

  cat("\n常规包状态：\n")

  # Performance optimization: Get all installed packages at once
  # Remove duplicates by keeping only the first occurrence of each package
  installed_pkgs_matrix <- installed.packages()[, c("Package", "Version")]
  installed_pkgs_info <- as.data.frame(installed_pkgs_matrix, stringsAsFactors = FALSE)
  # Remove duplicate packages (keep first occurrence which has highest priority)
  installed_pkgs_info <- installed_pkgs_info[!duplicated(installed_pkgs_info$Package), ]
  rownames(installed_pkgs_info) <- installed_pkgs_info$Package

  installed_essential <- c()
  missing_essential <- c()
  outdated_essential <- c()

  for (pkg_name in names(package_defs$essential_packages)) {
    recommended_version <- package_defs$essential_packages[[pkg_name]]

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      missing_essential <- c(missing_essential, pkg_name)
    } else {
      # Use cached package info instead of calling packageVersion()
      current_version <- installed_pkgs_info[pkg_name, "Version"]
      version_comparison <- compareVersion(current_version, recommended_version)

      if (version_comparison < 0) {
        outdated_essential <- c(outdated_essential, pkg_name)
        installed_essential <- c(installed_essential, pkg_name)
      } else {
        installed_essential <- c(installed_essential, pkg_name)
      }
    }
  }

  if (length(missing_essential) == 0 && length(outdated_essential) == 0) {
    cat("所有常规包已正确安装\n")
  } else {
    if (length(missing_essential) > 0) {
      cat(sprintf("缺少 %d 个包未安装\n", length(missing_essential)))
    }

    if (length(outdated_essential) > 0) {
      cat(sprintf("有 %d 个包版本过低\n", length(outdated_essential)))
    }
  }

  cat("-----------------------------------\n")

  packages_to_install <- c(missing_essential, outdated_essential)

  return(list(
    version_ok = version_ok,
    base_missing = base_missing,
    installed_essential = installed_essential,
    missing_essential = missing_essential,
    outdated_essential = outdated_essential,
    packages_to_install = packages_to_install
  ))
}

#' Install essential packages
#' @param pkg_analysis analysis results
#' @param package_defs definitions
#' @param interactive interactive mode
#' @return installation results
#' @keywords internal
install_essential_packages <- function(pkg_analysis, package_defs, interactive) {
  packages_to_install <- pkg_analysis$packages_to_install
  missing_essential <- pkg_analysis$missing_essential
  outdated_essential <- pkg_analysis$outdated_essential

  # Determine package type based on OS for robust installation
  os_type <- Sys.info()["sysname"]
  pkg_type <- if(os_type == "Windows") "binary" else "both"

  if (length(packages_to_install) > 0) {
    cat(sprintf("\n即将为您安装/更新 %d 个常规包\n", length(packages_to_install)))
    if (length(missing_essential) > 0) {
      cat(sprintf("- 缺失包：%d 个\n", length(missing_essential)))
    }
    if (length(outdated_essential) > 0) {
      cat(sprintf("- 需更新包：%d 个\n", length(outdated_essential)))
    }
    cat("这些包是使用R语言进行科研的基础, 且占用较小, 建议继续安装\n")
    cat(sprintf("安装路径：%s\n\n", .libPaths()[1]))

    if (interactive) {
      choice <- readline("是否继续安装/更新？(Y/yes), 输入其他任意内容退出本程序: ")
      if (tolower(choice) %in% c("y", "yes", "")) {
      } else {
        cat("安装已取消，程序已退出。\n")
        return(list(status = "cancelled"))
      }
    }

    cat("=== 正在安装/更新常规包 ===\n")
    cat("提示：安装过程会自动处理网络超时和重试，请耐心等待\n\n")

    # Start timing
    start_time <- Sys.time()

    # Pre-generate progress bar strings for milestones
    milestones <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1.00)
    progress_bars <- list(
      "10" = list(bar = paste(rep("█", 2), collapse = ""), empty = paste(rep("░", 18), collapse = ""), percent = 10),
      "25" = list(bar = paste(rep("█", 5), collapse = ""), empty = paste(rep("░", 15), collapse = ""), percent = 25),
      "50" = list(bar = paste(rep("█", 10), collapse = ""), empty = paste(rep("░", 10), collapse = ""), percent = 50),
      "75" = list(bar = paste(rep("█", 15), collapse = ""), empty = paste(rep("░", 5), collapse = ""), percent = 75),
      "90" = list(bar = paste(rep("█", 18), collapse = ""), empty = paste(rep("░", 2), collapse = ""), percent = 90),
      "95" = list(bar = paste(rep("█", 19), collapse = ""), empty = paste(rep("░", 1), collapse = ""), percent = 95),
      "100" = list(bar = paste(rep("█", 20), collapse = ""), empty = "", percent = 100)
    )
    last_milestone <- 0

    success_count <- 0
    failed_packages <- list()  # Changed to list to store error details

    for (i in seq_along(packages_to_install)) {
      pkg_name <- packages_to_install[i]
      recommended_version <- package_defs$essential_packages[[pkg_name]]

      cat(sprintf("[%d/%d] 安装 %s (推荐版本 %s)...",
                  i, length(packages_to_install), pkg_name, recommended_version))

      # Use new retry mechanism
      install_result <- install_package_with_retry(pkg_name, recommended_version, pkg_type)

      if (install_result$success) {
        retry_info <- if (install_result$attempt > 1) {
          sprintf(" (第%d次尝试成功)", install_result$attempt)
        } else {
          ""
        }

        source_info <- if (!is.null(install_result$note) && install_result$note == "installed_from_source") {
          " [源码编译]"
        } else {
          ""
        }

        cat(sprintf(" 完成%s%s\n", retry_info, source_info))
        success_count <- success_count + 1

        if (pkg_name %in% missing_essential) {
          missing_essential <- missing_essential[missing_essential != pkg_name]
        }
        if (pkg_name %in% outdated_essential) {
          outdated_essential <- outdated_essential[outdated_essential != pkg_name]
        }
      } else {
        # Classify error with version info for manual command generation
        error_info <- classify_install_error(install_result$error, pkg_name, recommended_version)
        cat(sprintf(" 失败 (%s)\n", error_info$type_cn))

        # Store detailed error information
        failed_packages[[pkg_name]] <- list(
          error_type = error_info$type,
          error_type_cn = error_info$type_cn,
          suggestion = error_info$suggestion,
          detail = error_info$detail,
          recommended_version = recommended_version,
          manual_command = error_info$manual_command
        )
      }

      # Progress bar - only update at key milestones (10%, 25%, 50%, 75%, 90%, 95%, 100%)
      current_progress <- i / length(packages_to_install)
      for (m in milestones) {
        if (current_progress >= m && last_milestone < m) {
          milestone_key <- as.character(m * 100)
          pb <- progress_bars[[milestone_key]]
          cat(sprintf("总进度: %s%s %d%%\n\n", pb$bar, pb$empty, pb$percent))
          last_milestone <- m
          break
        }
      }
    }

    # Calculate elapsed time
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    elapsed_mins <- floor(as.numeric(elapsed_time) / 60)
    elapsed_secs <- round(as.numeric(elapsed_time) %% 60)

    cat(sprintf("\n=== 安装完成 ===\n"))
    cat(sprintf("成功：%d 个，失败：%d 个\n", success_count, length(failed_packages)))
    cat(sprintf("总耗时：%d分%d秒\n", elapsed_mins, elapsed_secs))

    if (length(failed_packages) > 0) {
      cat("\n--- 安装失败详情 ---\n")

      # Group errors by type
      error_types <- sapply(failed_packages, function(x) x$error_type)
      unique_types <- unique(error_types)

      for (err_type in unique_types) {
        pkgs_with_this_error <- names(failed_packages)[error_types == err_type]
        if (length(pkgs_with_this_error) > 0) {
          first_pkg <- pkgs_with_this_error[1]
          error_info <- failed_packages[[first_pkg]]

          cat(sprintf("\n【%s】(%d个包)\n", error_info$error_type_cn, length(pkgs_with_this_error)))
          for (pkg in pkgs_with_this_error) {
            cat(sprintf("  • %s (v%s)\n", pkg, failed_packages[[pkg]]$recommended_version))
          }
          cat(sprintf("  原因：%s\n", error_info$detail))
          cat(sprintf("  建议：%s\n", error_info$suggestion))
        }
      }

      cat("\n提示：您可以稍后手动安装失败的包，或重新运行 check_packages() 再次尝试\n")
    }

    return(list(
      status = "completed",
      success_count = success_count,
      failed_packages = names(failed_packages),  # Return package names for compatibility
      failed_packages_details = failed_packages,  # Detailed error information
      missing_essential = missing_essential,
      outdated_essential = outdated_essential
    ))
  }

  return(list(status = "no_install_needed"))
}
