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

#' Persist library path configuration
#' @param lib_path Library path to persist
#' @return list with success status
#' @keywords internal
persist_library_path <- function(lib_path) {
  success <- FALSE
  methods_tried <- c()

  # Method 1: .Rprofile (most reliable, user-level)
  tryCatch({
    rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")

    # Read existing content
    existing_content <- if (file.exists(rprofile_path)) {
      readLines(rprofile_path, warn = FALSE)
    } else {
      character(0)
    }

    # Check if already configured
    lib_path_normalized <- normalizePath(lib_path, winslash = "/", mustWork = FALSE)
    pattern <- paste0("^\\.libPaths\\(.*", gsub("\\\\", "\\\\\\\\", lib_path_normalized))
    already_configured <- any(grepl(pattern, existing_content))

    if (!already_configured) {
      # Add library path configuration
      new_lines <- c(
        "",
        "# Auto-configured by packagehelp",
        sprintf('.libPaths(c("%s", .libPaths()))', lib_path_normalized),
        ""
      )

      # Write to .Rprofile
      writeLines(c(existing_content, new_lines), rprofile_path)
      success <- TRUE
      methods_tried <- c(methods_tried, ".Rprofile")
    } else {
      success <- TRUE
      methods_tried <- c(methods_tried, ".Rprofile (already configured)")
    }
  }, error = function(e) {
    methods_tried <<- c(methods_tried, ".Rprofile (failed)")
  })

  # Method 2: R_LIBS_USER environment variable (fallback)
  if (!success) {
    tryCatch({
      renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

      existing_content <- if (file.exists(renviron_path)) {
        readLines(renviron_path, warn = FALSE)
      } else {
        character(0)
      }

      # Remove existing R_LIBS_USER if present
      existing_content <- existing_content[!grepl("^R_LIBS_USER=", existing_content)]

      # Add new R_LIBS_USER
      lib_path_normalized <- normalizePath(lib_path, winslash = "/", mustWork = FALSE)
      new_line <- sprintf('R_LIBS_USER="%s"', lib_path_normalized)

      writeLines(c(existing_content, "", "# Auto-configured by packagehelp", new_line), renviron_path)
      success <- TRUE
      methods_tried <- c(methods_tried, ".Renviron")
    }, error = function(e) {
      methods_tried <- c(methods_tried, ".Renviron (failed)")
    })
  }

  return(list(
    success = success,
    methods = methods_tried
  ))
}

#' Generate environment report
#' @param sys_config system configuration
#' @return environment information
#' @keywords internal
generate_environment_report <- function(sys_config) {
  cat("=== R包环境检查报告 ===\n")
  sys_info <- Sys.info()
  r_version <- getRversion()
  total_packages <- length(installed.packages()[,1])

  cat(sprintf("系统信息：%s %s\n", sys_info["sysname"], sys_info["release"]))
  cat(sprintf("当前 R 版本：R %s\n", r_version))
  cat(sprintf("当前已安装 R 包：%d 个\n", total_packages))

  lib_paths <- .libPaths()
  cat("R包安装目录：\n")
  for (i in seq_along(lib_paths)) {
    cat(sprintf("  [%d] %s\n", i, lib_paths[i]))
  }

  has_system_path <- any(grepl(sys_config$system_drive_pattern, lib_paths, ignore.case = TRUE))

  cat("\n您现有的R包均安装在上述目录，如果出现多个，通常默认安装在第一个。\n")
  cat("\n接下来将协助您安装常用R包，请按照提示操作即可。\n\n")
  if (has_system_path) {
    cat(sprintf("检测到包安装在%s，如果想修改为%s路径，请选择B：\n\n", sys_config$system_drive_name, sys_config$non_system_desc))
  } else {
    cat("如需修改包安装路径，请选择：\n\n")
  }

  return(list(
    r_version = r_version,
    total_packages = total_packages,
    lib_paths = lib_paths,
    has_system_path = has_system_path
  ))
}

#' Handle library path configuration
#' @param interactive interactive mode
#' @param sys_config system configuration
#' @return path
#' @keywords internal
handle_library_path_configuration <- function(interactive, sys_config) {
  initial_lib_paths <- .libPaths()

  if (interactive) {
    cat("A. 保持现有设置，安装在上述目录\n")
    cat(sprintf("B. 增加一个新的安装路径并设置为优先路径，以后R包都会安装到此路径（推荐，不影响现有包）\n\n"))

    path_choice <- readline("请选择 (A/B): ")

    if (toupper(path_choice) == "A") {
      cat("已选择保持现有设置。\n\n")
    } else if (toupper(path_choice) == "B") {
      cat("\n=== 添加新的R包安装路径 ===\n")
      cat(sprintf("建议选择%s的路径，以下是一些推荐选项：\n", sys_config$non_system_desc))
      cat(sprintf("1. %s\n", sys_config$system_paths$option1))
      cat(sprintf("2. %s\n", sys_config$system_paths$option2))
      cat(sprintf("3. %s\n", sys_config$system_paths$option3))
      cat("4. 自定义路径\n\n")

      path_option <- readline("请选择路径选项 (1-4): ")

      new_path <- NULL
      if (path_option == "1") {
        new_path <- sys_config$system_paths$option1
      } else if (path_option == "2") {
        new_path <- sys_config$system_paths$option2
      } else if (path_option == "3") {
        new_path <- sys_config$system_paths$option3
      } else if (path_option == "4") {
        cat("请输入您的自定义路径: ")
        new_path <- readline()

        if (new_path == "" || is.null(new_path)) {
          cat("未输入路径，程序已退出。\n")
          return(list(status = "cancelled", reason = "empty_path"))
        }

        if (startsWith(new_path, "~")) {
          new_path <- path.expand(new_path)
        }

        if (!sys_config$validate_path(new_path)) {
          cat("输入的路径格式无效，程序已退出。\n")
          cat(sprintf("请输入完整路径，例如：%s\n", sys_config$path_examples))
          return(list(status = "cancelled", reason = "invalid_path_format"))
        }
      } else {
        cat("无效选择，程序已退出。\n")
        return(list(status = "cancelled", reason = "invalid_path_option"))
      }

      if (new_path != "" && !is.null(new_path)) {
        if (startsWith(new_path, "~")) {
          new_path <- path.expand(new_path)
        }

        new_path <- normalizePath(new_path, mustWork = FALSE)

        if (!dir.exists(new_path)) {
          cat(sprintf("路径 %s 不存在，正在创建...\n", new_path))
          tryCatch({
            dir.create(new_path, recursive = TRUE)
            cat("路径创建成功！\n")
          }, error = function(e) {
            cat(sprintf("路径创建失败：%s\n", e$message))
            cat("程序已退出。\n")
            return(list(status = "cancelled", reason = "path_creation_failed"))
          })
        }

        if (!is.null(new_path) && dir.exists(new_path)) {
          .libPaths(c(new_path, .libPaths()))
          cat(sprintf("已将 %s 添加为优先安装路径。\n", new_path))
          cat("新安装的包将优先安装到此路径。\n")

          # Make the configuration persistent across R sessions
          persist_result <- persist_library_path(new_path)
          if (persist_result$success) {
            cat("✓ 路径已永久保存，重启R后仍然有效。\n\n")
          } else {
            cat("⚠ 注意：当前会话已生效，但重启R后需重新设置。\n\n")
          }
        }
      } else {
        cat("未输入有效路径，程序已退出。\n")
        return(list(status = "cancelled", reason = "invalid_path_input"))
      }
    } else {
      cat("无效选择，程序已退出。\n")
      return(list(status = "cancelled", reason = "invalid_choice"))
    }

    updated_paths <- .libPaths()
    if (!identical(initial_lib_paths, updated_paths)) {
      cat("\n更新后的R包安装目录：\n")
      for (i in seq_along(updated_paths)) {
        is_new <- !(updated_paths[i] %in% initial_lib_paths)
        marker <- if (is_new) " (新增)" else ""
        cat(sprintf("  [%d] %s%s\n", i, updated_paths[i], marker))
      }
      cat("\n")
    }
  }

  return(list(status = "success"))
}

#' Install optional packages
#' @param optional_packages optional
#' @param interactive interactive mode
#' @return results
#' @keywords internal
install_optional_packages <- function(optional_packages, interactive) {
  # Determine package type based on OS for robust installation
  os_type <- Sys.info()["sysname"]
  pkg_type <- if(os_type == "Windows") "binary" else "both"

  # Helper function to check package status
  check_package_status <- function(pkg_name, recommended_version) {
    # Define critical packages that need STRICT version control
    critical_packages <- c("ggplot2", "rms")

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      return(list(status = "not_installed", action = "install"))
    }

    # Use packageVersion() directly - more reliable
    current_version <- as.character(packageVersion(pkg_name))

    # Check if package is loaded
    if (pkg_name %in% loadedNamespaces()) {
      version_compare <- tryCatch({
        compareVersion(current_version, recommended_version)
      }, error = function(e) NA)

      if (is.na(version_compare)) {
        return(list(status = "loaded_unknown_version",
                   action = "skip",
                   current_version = current_version,
                   recommended_version = recommended_version))
      } else if (version_compare != 0) {
        # Version mismatch while loaded
        return(list(status = "loaded_version_mismatch",
                   action = "skip",
                   current_version = current_version,
                   recommended_version = recommended_version,
                   is_critical = pkg_name %in% critical_packages))
      } else {
        return(list(status = "loaded_current", action = "skip"))
      }
    }

    # Package installed but not loaded - check version
    version_compare <- tryCatch({
      compareVersion(current_version, recommended_version)
    }, error = function(e) NA)

    if (is.na(version_compare)) {
      return(list(status = "unknown_version",
                 action = "skip",
                 current_version = current_version,
                 recommended_version = recommended_version))
    } else if (version_compare != 0) {
      # Version mismatch - different handling for critical vs non-critical
      return(list(status = "version_mismatch",
                 action = "skip",
                 current_version = current_version,
                 recommended_version = recommended_version,
                 is_critical = pkg_name %in% critical_packages))
    } else {
      return(list(status = "current", action = "skip"))
    }
  }

  # Helper function to install or update package (now uses retry mechanism)
  install_or_update_package <- function(pkg_name, recommended_version, action) {
    if (action == "skip") {
      return(list(success = TRUE, skipped = TRUE))
    }

    # Use the new retry mechanism
    result <- install_package_with_retry(pkg_name, recommended_version, pkg_type)
    return(result)
  }

  if (interactive) {
    cat("\n=== 功能扩展包安装 ===\n")
    cat("以下是可选的功能包类别：\n\n")

    for (i in seq_along(optional_packages)) {
      pkg_count <- length(names(optional_packages[[i]]))
      cat(sprintf("%d. %s (%d个包)\n", i, names(optional_packages)[i], pkg_count))
    }

    cat("\n选择安装方式：\n")
    cat("A. 全部安装 (推荐)\n")
    cat("B. 自定义选择\n")
    cat("C. 暂不安装\n\n")

    choice <- readline("请输入选择 (A/B/C): ")

    if (toupper(choice) == "A") {
      # Flatten the nested list and remove category prefixes from names
      all_optional <- unlist(optional_packages, recursive = FALSE)
      # Remove category prefix (e.g., "机器学习.xgboost" -> "xgboost")
      names(all_optional) <- sub("^[^.]+\\.", "", names(all_optional))
      cat(sprintf("正在分析 %d 个功能扩展包状态...\n", length(all_optional)))

      # Analyze package status first
      packages_to_install <- list()
      packages_loaded <- list()
      packages_version_mismatch <- list()  # Packages with version mismatch (not loaded)
      packages_skipped <- 0

      for (pkg_name in names(all_optional)) {
        recommended_version <- all_optional[[pkg_name]]
        status <- check_package_status(pkg_name, recommended_version)

        if (status$action == "install") {
          packages_to_install[[pkg_name]] <- recommended_version
        } else if (status$status %in% c("loaded_unknown_version", "loaded_version_mismatch")) {
          packages_loaded[[pkg_name]] <- list(
            recommended = recommended_version,
            current = status$current_version,
            is_critical = if (!is.null(status$is_critical)) status$is_critical else FALSE
          )
        } else if (status$status %in% c("unknown_version", "version_mismatch")) {
          packages_version_mismatch[[pkg_name]] <- list(
            recommended = recommended_version,
            current = status$current_version,
            is_critical = if (!is.null(status$is_critical)) status$is_critical else FALSE
          )
        } else {
          packages_skipped <- packages_skipped + 1
        }
      }

      # Report analysis results
      cat("\n分析结果：\n")
      if (packages_skipped > 0) {
        cat(sprintf("  - 已是推荐版本：%d 个包\n", packages_skipped))
      }
      if (length(packages_to_install) > 0) {
        cat(sprintf("  - 需要安装：%d 个包\n", length(packages_to_install)))
      }
      if (length(packages_loaded) > 0) {
        cat(sprintf("  - 已加载（版本不一致）：%d 个包\n", length(packages_loaded)))
      }
      if (length(packages_version_mismatch) > 0) {
        cat(sprintf("  - ℹ 版本不一致（未加载）：%d 个包\n", length(packages_version_mismatch)))
      }

      # Process packages to install
      total_to_process <- length(packages_to_install)
      if (total_to_process > 0) {
        cat(sprintf("\n开始安装 %d 个缺失的包...\n", total_to_process))
        cat("提示：安装过程会自动处理网络超时和重试，请耐心等待\n\n")

        # Start timing
        start_time <- Sys.time()

        pkg_counter <- 0
        failed_optional <- list()  # Track failures with details

        # Install missing packages
        for (pkg_name in names(packages_to_install)) {
          pkg_counter <- pkg_counter + 1
          recommended_version <- packages_to_install[[pkg_name]]
          cat(sprintf("[%d/%d] 安装 %s (v%s)...",
                     pkg_counter, total_to_process, pkg_name, recommended_version))

          install_result <- install_or_update_package(pkg_name, recommended_version, "install")

          if (install_result$success) {
            retry_info <- if (!is.null(install_result$attempt) && install_result$attempt > 1) {
              sprintf(" (第%d次尝试成功)", install_result$attempt)
            } else {
              ""
            }
            cat(sprintf(" 完成%s\n", retry_info))
          } else {
            error_info <- classify_install_error(install_result$error, pkg_name, recommended_version)
            cat(sprintf(" 失败 (%s)\n", error_info$type_cn))
            failed_optional[[pkg_name]] <- list(
              error_type = error_info$type,
              error_type_cn = error_info$type_cn,
              suggestion = error_info$suggestion,
              recommended_version = recommended_version,
              manual_command = error_info$manual_command
            )
          }
        }

        # Calculate elapsed time
        end_time <- Sys.time()
        elapsed_time <- difftime(end_time, start_time, units = "secs")
        elapsed_mins <- floor(as.numeric(elapsed_time) / 60)
        elapsed_secs <- round(as.numeric(elapsed_time) %% 60)

        cat(sprintf("\n功能包安装完成，总耗时：%d分%d秒\n", elapsed_mins, elapsed_secs))

        # Report failures if any
        if (length(failed_optional) > 0) {
          cat(sprintf("\n有 %d 个功能包安装失败，详情：\n", length(failed_optional)))
          for (pkg in names(failed_optional)) {
            err <- failed_optional[[pkg]]
            cat(sprintf("  • %s (v%s) - %s\n", pkg, err$recommended_version, err$error_type_cn))
            cat(sprintf("    建议: %s\n", err$suggestion))
          }
        }
      }

      # Report loaded packages with version mismatch
      if (length(packages_loaded) > 0) {
        cat("\n以下包已加载，版本不一致（需重启R后处理）：\n")
        for (pkg_name in names(packages_loaded)) {
          pkg_info <- packages_loaded[[pkg_name]]
          cat(sprintf("  - %s: 当前 %s，推荐 %s",
                     pkg_name, pkg_info$current, pkg_info$recommended))
          if (isTRUE(pkg_info$is_critical)) {
            cat(" [关键包]")
          }
          cat("\n")
        }
      }

      # Report non-loaded packages with version mismatch
      if (length(packages_version_mismatch) > 0) {
        # Separate critical and non-critical
        critical_mismatches <- names(packages_version_mismatch)[sapply(packages_version_mismatch, function(x) isTRUE(x$is_critical))]
        non_critical_mismatches <- setdiff(names(packages_version_mismatch), critical_mismatches)

        if (length(critical_mismatches) > 0) {
          cat("\n⚠⚠⚠ 关键包版本不一致警告 ⚠⚠⚠\n")
          cat("以下包使用其他版本可能存在兼容性风险，建议卸载后重新安装指定版本：\n")
          for (pkg_name in critical_mismatches) {
            pkg_info <- packages_version_mismatch[[pkg_name]]
            cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                       pkg_name, pkg_info$current, pkg_info$recommended))
            cat(sprintf("    卸载命令：remove.packages(\"%s\")\n", pkg_name))
          }
        }

        if (length(non_critical_mismatches) > 0) {
          cat("\nℹ 以下包版本与推荐版本不一致：\n")
          for (pkg_name in non_critical_mismatches) {
            pkg_info <- packages_version_mismatch[[pkg_name]]
            cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                       pkg_name, pkg_info$current, pkg_info$recommended))
          }
          cat("提示：如遇兼容性问题，建议更新R包为推荐版本或联系客服\n")
        }
      }
    } else if (toupper(choice) == "B") {
      cat("请选择要安装的功能包类别（可多选）：\n")
      for (i in seq_along(optional_packages)) {
        cat(sprintf("[%d] %s\n", i, names(optional_packages)[i]))
      }
      cat("\n输入示例：输入 1,3 安装第1和第3类，或直接回车跳过\n")

      user_choice <- readline("您的选择：")
      if (user_choice != "" && !is.null(user_choice)) {
        selected_nums <- as.numeric(unlist(strsplit(gsub(" ", "", user_choice), ",")))
        selected_nums <- selected_nums[!is.na(selected_nums) & selected_nums %in% 1:length(optional_packages)]

        if (length(selected_nums) > 0) {
          selected_packages <- list()
          for (num in selected_nums) {
            selected_packages <- c(selected_packages, optional_packages[[num]])
          }

          cat(sprintf("正在分析选中的 %d 个功能包状态...\n", length(selected_packages)))

          # Analyze package status first
          packages_to_install <- list()
          packages_loaded <- list()
          packages_version_mismatch <- list()
          packages_skipped <- 0

          for (pkg_name in names(selected_packages)) {
            recommended_version <- selected_packages[[pkg_name]]
            status <- check_package_status(pkg_name, recommended_version)

            if (status$action == "install") {
              packages_to_install[[pkg_name]] <- recommended_version
            } else if (status$status %in% c("loaded_unknown_version", "loaded_version_mismatch")) {
              packages_loaded[[pkg_name]] <- list(
                recommended = recommended_version,
                current = status$current_version,
                is_critical = if (!is.null(status$is_critical)) status$is_critical else FALSE
              )
            } else if (status$status %in% c("unknown_version", "version_mismatch")) {
              packages_version_mismatch[[pkg_name]] <- list(
                recommended = recommended_version,
                current = status$current_version,
                is_critical = if (!is.null(status$is_critical)) status$is_critical else FALSE
              )
            } else {
              packages_skipped <- packages_skipped + 1
            }
          }

          # Report analysis results
          cat("\n分析结果：\n")
          if (packages_skipped > 0) {
            cat(sprintf("  - 已是推荐版本：%d 个包\n", packages_skipped))
          }
          if (length(packages_to_install) > 0) {
            cat(sprintf("  - 需要安装：%d 个包\n", length(packages_to_install)))
          }
          if (length(packages_loaded) > 0) {
            cat(sprintf("  - 已加载（版本不一致）：%d 个包\n", length(packages_loaded)))
          }
          if (length(packages_version_mismatch) > 0) {
            cat(sprintf("  - ℹ 版本不一致（未加载）：%d 个包\n", length(packages_version_mismatch)))
          }

          # Process packages to install
          total_to_process <- length(packages_to_install)
          if (total_to_process > 0) {
            cat(sprintf("\n开始安装 %d 个缺失的包...\n", total_to_process))
            cat("提示：安装过程会自动处理网络超时和重试，请耐心等待\n\n")

            # Start timing
            start_time <- Sys.time()

            pkg_counter <- 0
            failed_optional_custom <- list()  # Track failures with details

            # Install missing packages
            for (pkg_name in names(packages_to_install)) {
              pkg_counter <- pkg_counter + 1
              recommended_version <- packages_to_install[[pkg_name]]
              cat(sprintf("[%d/%d] 安装 %s (v%s)...",
                         pkg_counter, total_to_process, pkg_name, recommended_version))

              install_result <- install_or_update_package(pkg_name, recommended_version, "install")

              if (install_result$success) {
                retry_info <- if (!is.null(install_result$attempt) && install_result$attempt > 1) {
                  sprintf(" (第%d次尝试成功)", install_result$attempt)
                } else {
                  ""
                }
                cat(sprintf(" 完成%s\n", retry_info))
              } else {
                error_info <- classify_install_error(install_result$error, pkg_name, recommended_version)
                cat(sprintf(" 失败 (%s)\n", error_info$type_cn))
                failed_optional_custom[[pkg_name]] <- list(
                  error_type = error_info$type,
                  error_type_cn = error_info$type_cn,
                  suggestion = error_info$suggestion,
                  recommended_version = recommended_version,
                  manual_command = error_info$manual_command
                )
              }
            }

            # Calculate elapsed time
            end_time <- Sys.time()
            elapsed_time <- difftime(end_time, start_time, units = "secs")
            elapsed_mins <- floor(as.numeric(elapsed_time) / 60)
            elapsed_secs <- round(as.numeric(elapsed_time) %% 60)

            cat(sprintf("\n功能包安装完成，总耗时：%d分%d秒\n", elapsed_mins, elapsed_secs))

            # Report failures if any
            if (length(failed_optional_custom) > 0) {
              cat(sprintf("\n有 %d 个功能包安装失败，详情：\n", length(failed_optional_custom)))
              for (pkg in names(failed_optional_custom)) {
                err <- failed_optional_custom[[pkg]]
                cat(sprintf("  • %s (v%s) - %s\n", pkg, err$recommended_version, err$error_type_cn))
                cat(sprintf("    建议: %s\n", err$suggestion))
              }
            }
          }

          # Report loaded packages with version mismatch
          if (length(packages_loaded) > 0) {
            cat("\n以下包已加载，版本不一致（需重启R后处理）：\n")
            for (pkg_name in names(packages_loaded)) {
              pkg_info <- packages_loaded[[pkg_name]]
              cat(sprintf("  - %s: 当前 %s，推荐 %s",
                         pkg_name, pkg_info$current, pkg_info$recommended))
              if (isTRUE(pkg_info$is_critical)) {
                cat(" [关键包]")
              }
              cat("\n")
            }
          }

          # Report non-loaded packages with version mismatch
          if (length(packages_version_mismatch) > 0) {
            # Separate critical and non-critical
            critical_mismatches <- names(packages_version_mismatch)[sapply(packages_version_mismatch, function(x) isTRUE(x$is_critical))]
            non_critical_mismatches <- setdiff(names(packages_version_mismatch), critical_mismatches)

            if (length(critical_mismatches) > 0) {
              cat("\n⚠⚠⚠ 关键包版本不一致警告 ⚠⚠⚠\n")
              cat("以下包使用其他版本可能存在兼容性风险，建议卸载后重新安装指定版本：\n")
              for (pkg_name in critical_mismatches) {
                pkg_info <- packages_version_mismatch[[pkg_name]]
                cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                           pkg_name, pkg_info$current, pkg_info$recommended))
                cat(sprintf("    卸载命令：remove.packages(\"%s\")\n", pkg_name))
              }
            }

            if (length(non_critical_mismatches) > 0) {
              cat("\nℹ 以下包版本与推荐版本不一致：\n")
              for (pkg_name in non_critical_mismatches) {
                pkg_info <- packages_version_mismatch[[pkg_name]]
                cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                           pkg_name, pkg_info$current, pkg_info$recommended))
              }
              cat("提示：如遇兼容性问题，建议更新R包为推荐版本或联系客服\n")
            }
          }
        } else {
          cat("未选择有效的功能包类别。\n")
        }
      } else {
        cat("已跳过功能扩展包安装。\n")
      }
    } else if (toupper(choice) == "C") {
      cat("已跳过功能扩展包安装。\n")
    } else {
      cat("无效选择，已跳过功能扩展包安装。\n")
    }
  }

  return(list(status = "completed"))
}

#' Generate final report
#' @param package_defs definitions
#' @param pkg_analysis analysis results
#' @return final results
#' @keywords internal
generate_final_report <- function(package_defs, pkg_analysis) {
  cat("\n=== 环境检查和配置完成 ===\n")

  final_installed <- 0
  final_missing <- c()

  for (pkg_name in names(package_defs$essential_packages)) {
    if (requireNamespace(pkg_name, quietly = TRUE)) {
      final_installed <- final_installed + 1
    } else {
      final_missing <- c(final_missing, pkg_name)
    }
  }

  has_critical_mismatch <- length(pkg_analysis$critical_version_mismatch) > 0

  if (final_installed == length(package_defs$essential_packages)) {
    cat("常规包：全部已安装")
    if (has_critical_mismatch) {
      cat(sprintf(" (其中 %d 个关键包版本不一致)", length(pkg_analysis$critical_version_mismatch)))
    }
    cat("\n")
  } else {
    missing_count <- length(package_defs$essential_packages) - final_installed
    cat(sprintf("常规包：缺少 %d 个未安装\n", missing_count))
  }

  if (final_installed == length(package_defs$essential_packages) && !has_critical_mismatch) {
    cat("环境检查和配置完成！现在您可以：\n")
    cat("1. 开始使用已安装的R包进行分析\n")
    cat("2. 如需重新检查环境，运行 check_packages()\n")
  } else {
    cat("建议：\n")
    if (length(final_missing) > 0) {
      cat("1. 检查网络连接后重新运行 check_packages()\n")
    }
    if (has_critical_mismatch) {
      cat("2. 按照上述提示卸载关键包，然后重新运行 check_packages() 安装指定版本\n")
    }
    cat("遇到问题及时咨询客服\n")
  }

  cat("\n")

  return(list(
    installed = setdiff(names(package_defs$essential_packages), final_missing),
    missing = final_missing,
    critical_version_mismatch = pkg_analysis$critical_version_mismatch,
    version_mismatch_info = pkg_analysis$version_mismatch_info,
    total_packages = length(installed.packages()[,1])
  ))
}
