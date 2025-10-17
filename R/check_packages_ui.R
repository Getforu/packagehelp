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
          cat("新安装的包将优先安装到此路径。\n\n")
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

  # Performance optimization: Get all installed packages at once (cache for check_package_status)
  # Remove duplicates by keeping only the first occurrence of each package
  installed_pkgs_matrix <- installed.packages()[, c("Package", "Version")]
  installed_pkgs_cache <- as.data.frame(installed_pkgs_matrix, stringsAsFactors = FALSE)
  # Remove duplicate packages (keep first occurrence which has highest priority)
  installed_pkgs_cache <- installed_pkgs_cache[!duplicated(installed_pkgs_cache$Package), ]
  rownames(installed_pkgs_cache) <- installed_pkgs_cache$Package

  # Helper function to check package status
  check_package_status <- function(pkg_name, recommended_version) {
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      return(list(status = "not_installed", action = "install"))
    }

    # Use cached package info instead of calling packageVersion()
    current_version <- installed_pkgs_cache[pkg_name, "Version"]

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
      } else if (version_compare < 0) {
        return(list(status = "loaded_outdated",
                   action = "skip",
                   current_version = current_version,
                   recommended_version = recommended_version))
      } else {
        return(list(status = "loaded_current", action = "skip"))
      }
    }

    # Package installed but not loaded
    version_compare <- tryCatch({
      compareVersion(current_version, recommended_version)
    }, error = function(e) NA)

    if (is.na(version_compare)) {
      return(list(status = "unknown_version",
                 action = "suggest_reinstall",
                 current_version = current_version,
                 recommended_version = recommended_version))
    } else if (version_compare < 0) {
      return(list(status = "outdated",
                 action = "update",
                 current_version = current_version,
                 recommended_version = recommended_version))
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
      packages_to_update <- list()
      packages_loaded <- list()
      packages_unknown_version <- list()
      packages_skipped <- 0

      for (pkg_name in names(all_optional)) {
        recommended_version <- all_optional[[pkg_name]]
        status <- check_package_status(pkg_name, recommended_version)

        if (status$action == "install") {
          packages_to_install[[pkg_name]] <- recommended_version
        } else if (status$action == "update") {
          packages_to_update[[pkg_name]] <- list(
            recommended = recommended_version,
            current = status$current_version
          )
        } else if (status$status %in% c("loaded_outdated", "loaded_unknown_version")) {
          packages_loaded[[pkg_name]] <- list(
            recommended = recommended_version,
            current = status$current_version
          )
        } else if (status$action == "suggest_reinstall") {
          packages_unknown_version[[pkg_name]] <- list(
            recommended = recommended_version,
            current = status$current_version
          )
        } else {
          packages_skipped <- packages_skipped + 1
        }
      }

      # Report analysis results
      cat("\n分析结果：\n")
      if (packages_skipped > 0) {
        cat(sprintf("  - 已是最新：%d 个包\n", packages_skipped))
      }
      if (length(packages_to_install) > 0) {
        cat(sprintf("  - 需要安装：%d 个包\n", length(packages_to_install)))
      }
      if (length(packages_to_update) > 0) {
        cat(sprintf("  - 需要更新：%d 个包\n", length(packages_to_update)))
      }
      if (length(packages_loaded) > 0) {
        cat(sprintf("  - 已加载无法更新：%d 个包\n", length(packages_loaded)))
      }
      if (length(packages_unknown_version) > 0) {
        cat(sprintf("  - 版本无法判断：%d 个包\n", length(packages_unknown_version)))
      }

      # Process packages to install/update
      total_to_process <- length(packages_to_install) + length(packages_to_update)
      if (total_to_process > 0) {
        cat(sprintf("\n开始处理 %d 个需要安装/更新的包...\n", total_to_process))
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

        # Update outdated packages
        for (pkg_name in names(packages_to_update)) {
          pkg_counter <- pkg_counter + 1
          pkg_info <- packages_to_update[[pkg_name]]
          cat(sprintf("[%d/%d] 更新 %s (%s -> %s)...",
                     pkg_counter, total_to_process, pkg_name,
                     pkg_info$current, pkg_info$recommended))

          install_result <- install_or_update_package(pkg_name, pkg_info$recommended, "update")

          if (install_result$success) {
            retry_info <- if (!is.null(install_result$attempt) && install_result$attempt > 1) {
              sprintf(" (第%d次尝试成功)", install_result$attempt)
            } else {
              ""
            }
            cat(sprintf(" 完成%s\n", retry_info))
          } else {
            error_info <- classify_install_error(install_result$error, pkg_name, pkg_info$recommended)
            cat(sprintf(" 失败 (%s)\n", error_info$type_cn))
            failed_optional[[pkg_name]] <- list(
              error_type = error_info$type,
              error_type_cn = error_info$type_cn,
              suggestion = error_info$suggestion,
              recommended_version = pkg_info$recommended,
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

      # Report loaded packages that need update
      if (length(packages_loaded) > 0) {
        cat("\n以下包已加载，无法更新（需重启R后更新）：\n")
        for (pkg_name in names(packages_loaded)) {
          pkg_info <- packages_loaded[[pkg_name]]
          cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                     pkg_name, pkg_info$current, pkg_info$recommended))
        }
      }

      # Report packages with unknown version
      if (length(packages_unknown_version) > 0) {
        cat("\n以下包版本无法判断，建议卸载后重新安装：\n")
        for (pkg_name in names(packages_unknown_version)) {
          pkg_info <- packages_unknown_version[[pkg_name]]
          cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                     pkg_name, pkg_info$current, pkg_info$recommended))
        }
        cat("使用 remove.packages(\"包名\") 卸载，然后重新运行安装\n")
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
          packages_to_update <- list()
          packages_loaded <- list()
          packages_unknown_version <- list()
          packages_skipped <- 0

          for (pkg_name in names(selected_packages)) {
            recommended_version <- selected_packages[[pkg_name]]
            status <- check_package_status(pkg_name, recommended_version)

            if (status$action == "install") {
              packages_to_install[[pkg_name]] <- recommended_version
            } else if (status$action == "update") {
              packages_to_update[[pkg_name]] <- list(
                recommended = recommended_version,
                current = status$current_version
              )
            } else if (status$status %in% c("loaded_outdated", "loaded_unknown_version")) {
              packages_loaded[[pkg_name]] <- list(
                recommended = recommended_version,
                current = status$current_version
              )
            } else if (status$action == "suggest_reinstall") {
              packages_unknown_version[[pkg_name]] <- list(
                recommended = recommended_version,
                current = status$current_version
              )
            } else {
              packages_skipped <- packages_skipped + 1
            }
          }

          # Report analysis results
          cat("\n分析结果：\n")
          if (packages_skipped > 0) {
            cat(sprintf("  - 已是最新：%d 个包\n", packages_skipped))
          }
          if (length(packages_to_install) > 0) {
            cat(sprintf("  - 需要安装：%d 个包\n", length(packages_to_install)))
          }
          if (length(packages_to_update) > 0) {
            cat(sprintf("  - 需要更新：%d 个包\n", length(packages_to_update)))
          }
          if (length(packages_loaded) > 0) {
            cat(sprintf("  - 已加载无法更新：%d 个包\n", length(packages_loaded)))
          }
          if (length(packages_unknown_version) > 0) {
            cat(sprintf("  - 版本无法判断：%d 个包\n", length(packages_unknown_version)))
          }

          # Process packages to install/update
          total_to_process <- length(packages_to_install) + length(packages_to_update)
          if (total_to_process > 0) {
            cat(sprintf("\n开始处理 %d 个需要安装/更新的包...\n", total_to_process))
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

            # Update outdated packages
            for (pkg_name in names(packages_to_update)) {
              pkg_counter <- pkg_counter + 1
              pkg_info <- packages_to_update[[pkg_name]]
              cat(sprintf("[%d/%d] 更新 %s (%s -> %s)...",
                         pkg_counter, total_to_process, pkg_name,
                         pkg_info$current, pkg_info$recommended))

              install_result <- install_or_update_package(pkg_name, pkg_info$recommended, "update")

              if (install_result$success) {
                retry_info <- if (!is.null(install_result$attempt) && install_result$attempt > 1) {
                  sprintf(" (第%d次尝试成功)", install_result$attempt)
                } else {
                  ""
                }
                cat(sprintf(" 完成%s\n", retry_info))
              } else {
                error_info <- classify_install_error(install_result$error, pkg_name, pkg_info$recommended)
                cat(sprintf(" 失败 (%s)\n", error_info$type_cn))
                failed_optional_custom[[pkg_name]] <- list(
                  error_type = error_info$type,
                  error_type_cn = error_info$type_cn,
                  suggestion = error_info$suggestion,
                  recommended_version = pkg_info$recommended,
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

          # Report loaded packages that need update
          if (length(packages_loaded) > 0) {
            cat("\n以下包已加载，无法更新（需重启R后更新）：\n")
            for (pkg_name in names(packages_loaded)) {
              pkg_info <- packages_loaded[[pkg_name]]
              cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                         pkg_name, pkg_info$current, pkg_info$recommended))
            }
          }

          # Report packages with unknown version
          if (length(packages_unknown_version) > 0) {
            cat("\n以下包版本无法判断，建议卸载后重新安装：\n")
            for (pkg_name in names(packages_unknown_version)) {
              pkg_info <- packages_unknown_version[[pkg_name]]
              cat(sprintf("  - %s: 当前 %s，推荐 %s\n",
                         pkg_name, pkg_info$current, pkg_info$recommended))
            }
            cat("使用 remove.packages(\"包名\") 卸载，然后重新运行安装\n")
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
  final_outdated <- 0
  final_missing <- c()

  for (pkg_name in names(package_defs$essential_packages)) {
    if (requireNamespace(pkg_name, quietly = TRUE)) {
      final_installed <- final_installed + 1
      current_version <- as.character(packageVersion(pkg_name))
      recommended_version <- package_defs$essential_packages[[pkg_name]]
      if (compareVersion(current_version, recommended_version) < 0) {
        final_outdated <- final_outdated + 1
      }
    } else {
      final_missing <- c(final_missing, pkg_name)
    }
  }

  if (final_installed == length(package_defs$essential_packages)) {
    cat("常规包：全部已安装")
    if (final_outdated > 0) {
      cat(sprintf(" (其中 %d 个版本过低)", final_outdated))
    }
    cat("\n")
  } else {
    missing_count <- length(package_defs$essential_packages) - final_installed
    cat(sprintf("常规包：缺少 %d 个未安装\n", missing_count))
  }

  if (final_installed == length(package_defs$essential_packages)) {
    cat("环境检查和配置完成！现在您可以：\n")
    cat("1. 开始使用已安装的R包进行分析\n")
    cat("2. 如需重新检查环境，运行 check_packages()\n")
    if (final_outdated > 0) {
      cat("3. 遇到问题及时咨询\n")
    }
  } else {
    cat("仍有部分包未安装，建议：\n")
    cat("1. 检查网络连接后重新运行 check_packages()\n")
    cat("2. 或手动安装缺失的包，遇到问题及时咨询\n")
  }

  cat("\n")

  return(list(
    installed = setdiff(names(package_defs$essential_packages), final_missing),
    missing = final_missing,
    outdated = pkg_analysis$outdated_essential,
    total_packages = length(installed.packages()[,1])
  ))
}
