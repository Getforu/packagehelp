# Copyright (C) 2025 packhelp Team
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

#' Initialize system configuration
#' @return list
#' @keywords internal
initialize_system_config <- function() {
  current_os <- Sys.info()["sysname"]
  is_windows <- current_os == "Windows"
  is_mac <- current_os == "Darwin"
  is_linux <- current_os == "Linux"
  
  if (is_windows) {
    system_paths <- list(
      option1 = "D:/R_packages",
      option2 = "E:/R_library", 
      option3 = "D:/Software/R_library"
    )
    path_examples <- "D:/R_packages 或 E:/Software/R_library"
    system_drive_pattern <- "^C:"
    system_drive_name <- "C盘"
    non_system_desc <- "非C盘"
    validate_path <- function(path) {
      nchar(path) >= 3 && grepl("^[A-Za-z]:", path)
    }
  } else if (is_mac) {
    home_dir <- path.expand("~")
    system_paths <- list(
      option1 = file.path(home_dir, "Documents", "R_packages"),
      option2 = file.path(home_dir, "Library", "R_library"),
      option3 = "/usr/local/lib/R_packages"
    )
    path_examples <- "~/Documents/R_packages 或 ~/Library/R_library"
    system_drive_pattern <- "^/System|^/usr/lib"
    system_drive_name <- "系统目录"
    non_system_desc <- "用户目录"
    validate_path <- function(path) {
      nchar(path) >= 2 && (startsWith(path, "/") || startsWith(path, "~"))
    }
  } else {
    home_dir <- path.expand("~")
    system_paths <- list(
      option1 = file.path(home_dir, "R_packages"),
      option2 = file.path(home_dir, ".local", "lib", "R"),
      option3 = "/opt/R_packages"
    )
    path_examples <- "~/R_packages 或 ~/.local/lib/R"
    system_drive_pattern <- "^/usr|^/lib|^/var"
    system_drive_name <- "系统目录"
    non_system_desc <- "用户目录"
    validate_path <- function(path) {
      nchar(path) >= 2 && (startsWith(path, "/") || startsWith(path, "~"))
    }
  }
  
  return(list(
    current_os = current_os,
    is_windows = is_windows,
    is_mac = is_mac,
    is_linux = is_linux,
    system_paths = system_paths,
    path_examples = path_examples,
    system_drive_pattern = system_drive_pattern,
    system_drive_name = system_drive_name,
    non_system_desc = non_system_desc,
    validate_path = validate_path
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
  
  installed_essential <- c()
  missing_essential <- c()
  outdated_essential <- c()
  
  for (pkg_name in names(package_defs$essential_packages)) {
    recommended_version <- package_defs$essential_packages[[pkg_name]]
    
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      missing_essential <- c(missing_essential, pkg_name)
    } else {
      current_version <- as.character(packageVersion(pkg_name))
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

#' Install
#' @param pkg_analysis analysis results
#' @param package_defs definitions
#' @param interactive interactive mode
#' @return installation results
#' @keywords internal
install_essential_packages <- function(pkg_analysis, package_defs, interactive) {
  packages_to_install <- pkg_analysis$packages_to_install
  missing_essential <- pkg_analysis$missing_essential
  outdated_essential <- pkg_analysis$outdated_essential
  
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
    success_count <- 0
    failed_packages <- c()
    
    for (i in seq_along(packages_to_install)) {
      pkg_name <- packages_to_install[i]
      recommended_version <- package_defs$essential_packages[[pkg_name]]
      
      cat(sprintf("正在安装/更新第 %d 个包及其依赖，请不要操作，全部R包安装完成后会提示已完成...", i))
      
      install_result <- tryCatch({
        if (requireNamespace("remotes", quietly = TRUE)) {
          remotes::install_version(pkg_name, version = recommended_version, 
                                 dependencies = TRUE, upgrade = "never", quiet = TRUE)
        } else {
          install.packages("remotes", quiet = TRUE)
          if (requireNamespace("remotes", quietly = TRUE)) {
            remotes::install_version(pkg_name, version = recommended_version, 
                                   dependencies = TRUE, upgrade = "never", quiet = TRUE)
          } else {
            install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
          }
        }
        
        if (requireNamespace(pkg_name, quietly = TRUE)) {
          TRUE
        } else {
          FALSE
        }
      }, error = function(e) {
        tryCatch({
          install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
          requireNamespace(pkg_name, quietly = TRUE)
        }, error = function(e2) {
          FALSE
        })
      })
      
      if (install_result) {
        cat(" 完成\n")
        success_count <- success_count + 1
        
        if (pkg_name %in% missing_essential) {
          missing_essential <- missing_essential[missing_essential != pkg_name]
        }
        if (pkg_name %in% outdated_essential) {
          outdated_essential <- outdated_essential[outdated_essential != pkg_name]
        }
      } else {
        cat(" 失败\n")
        failed_packages <- c(failed_packages, pkg_name)
      }
      
      progress <- round((i / length(packages_to_install)) * 100)
      progress_bar <- paste(rep("█", floor(progress/5)), collapse = "")
      progress_empty <- paste(rep("░", 20 - floor(progress/5)), collapse = "")
      cat(sprintf("总进度: %s%s %d%%\n", progress_bar, progress_empty, progress))
    }
    
    cat(sprintf("\n安装完成：成功 %d 个，失败 %d 个\n", success_count, length(failed_packages)))
    if (length(failed_packages) > 0) {
      cat("安装失败的包：\n")
      for (pkg in failed_packages) {
        cat(sprintf("  - %s (可稍后手动安装)\n", pkg))
      }
    }
    
    return(list(
      status = "completed",
      success_count = success_count,
      failed_packages = failed_packages,
      missing_essential = missing_essential,
      outdated_essential = outdated_essential
    ))
  }
  
  return(list(status = "no_install_needed"))
}

#' Install
#' @param optional_packages optional
#' @param interactive interactive mode
#' @return results
#' @keywords internal
install_optional_packages <- function(optional_packages, interactive) {
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
      all_optional <- unlist(optional_packages, recursive = FALSE)
      cat(sprintf("正在安装 %d 个功能扩展包...\n", length(all_optional)))
      pkg_counter <- 0
      for (pkg_name in names(all_optional)) {
        pkg_counter <- pkg_counter + 1
        recommended_version <- all_optional[[pkg_name]]
        cat(sprintf("正在安装/更新第 %d 个包及其依赖，请不要操作，全部R包安装完成后会提示已完成...", pkg_counter))
        
        install_result <- tryCatch({
          if (requireNamespace("remotes", quietly = TRUE)) {
            remotes::install_version(pkg_name, version = recommended_version, 
                                   dependencies = TRUE, upgrade = "never", quiet = TRUE)
          } else {
            install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
          }
          requireNamespace(pkg_name, quietly = TRUE)
        }, error = function(e) {
          tryCatch({
            install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
            requireNamespace(pkg_name, quietly = TRUE)
          }, error = function(e2) FALSE)
        })
        
        if (install_result) {
          cat(" 完成\n")
        } else {
          cat(" 失败\n")
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
          
          cat(sprintf("正在安装选中的 %d 个功能包...\n", length(selected_packages)))
          pkg_counter <- 0
          for (pkg_name in names(selected_packages)) {
            pkg_counter <- pkg_counter + 1
            recommended_version <- selected_packages[[pkg_name]]
            cat(sprintf("正在安装/更新第 %d 个包及其依赖，请不要操作，全部R包安装完成后会提示已完成...", pkg_counter))
            
            install_result <- tryCatch({
              if (requireNamespace("remotes", quietly = TRUE)) {
                remotes::install_version(pkg_name, version = recommended_version, 
                                       dependencies = TRUE, upgrade = "never", quiet = TRUE)
              } else {
                install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
              }
              requireNamespace(pkg_name, quietly = TRUE)
            }, error = function(e) {
              tryCatch({
                install.packages(pkg_name, dependencies = TRUE, quiet = TRUE)
                requireNamespace(pkg_name, quietly = TRUE)
              }, error = function(e2) FALSE)
            })
            
            if (install_result) {
              cat(" 完成\n")
            } else {
              cat(" 失败\n")
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

#' definitions
#' @return list
#' @keywords internal
get_package_definitions <- function() {
  min_r_version <- "4.0.0"
  base_packages <- c("base", "graphics", "grDevices", "grid", "stats", "utils")
  
  essential_packages <- list(
    "bit64" = "4.0.5",
    "digest" = "0.6.3.1", 
    "ggplot2" = "3.5.1",
    "gridExtra" = "2.3",
    "httr" = "1.4.7",
    "httr2" = "0.2.3",
    "jsonlite" = "1.8.7",
    "openssl" = "2.3.2",
    "Rcpp" = "1.0.14",
    "rstudioapi" = "0.15.0",
    "base64enc" = "0.1-3",
    "car" = "3.1-2",
    "dplyr" = "1.1.3",
    "gt" = "0.9.0",
    "magrittr" = "2.0.3",
    "mice" = "3.16.0",
    "openxlsx" = "4.2.5.2",
    "pROC" = "1.18.5",
    "rlang" = "1.1.1",
    "rms" = "6.7-1",
    "survival" = "3.6-4",
    "VIM" = "6.2.2"
  )
  
  optional_packages <- list(
    "机器学习" = list(
      "xgboost" = "1.7.8.1",
      "caret" = "6.0-94",
      "glmnet" = "4.1-8"
    ),
    "高级绘图" = list(
      "patchwork" = "1.1.2"
    ),
    "数据处理" = list(
      "data.table" = "1.14.8",
      "stringr" = "1.5.0",
      "lubridate" = "1.9.2"
    )
  )
  
  return(list(
    min_r_version = min_r_version,
    base_packages = base_packages,
    essential_packages = essential_packages,
    optional_packages = optional_packages
  ))
}

#' R Package Environment Check and Dependency Installation 
#' @param interactive interactive mode
#' @param install_missing auto install
#' @return Returns list
#' @export
check_packages <- function(interactive = TRUE, install_missing = TRUE) {
  package_defs <- get_package_definitions()
  
  sys_config <- initialize_system_config()
  
  env_report <- generate_environment_report(sys_config)
  
  path_result <- handle_library_path_configuration(interactive, sys_config)
  if (!is.null(path_result$status) && path_result$status == "cancelled") {
    return(invisible(path_result))
  }
  
  pkg_analysis <- analyze_package_status(package_defs)
  
  install_result <- NULL
  if (install_missing && length(pkg_analysis$packages_to_install) > 0) {
    install_result <- install_essential_packages(pkg_analysis, package_defs, interactive)
    if (!is.null(install_result$status) && install_result$status == "cancelled") {
      return(invisible(list(
        installed = pkg_analysis$installed_essential, 
        missing = pkg_analysis$missing_essential, 
        outdated = pkg_analysis$outdated_essential, 
        status = "cancelled"
      )))
    }
    if (!is.null(install_result$missing_essential)) {
      pkg_analysis$missing_essential <- install_result$missing_essential
    }
    if (!is.null(install_result$outdated_essential)) {
      pkg_analysis$outdated_essential <- install_result$outdated_essential
    }
  }
  
  if (interactive && length(pkg_analysis$missing_essential) == 0) {
    install_optional_packages(package_defs$optional_packages, interactive)
  }
  
  final_result <- generate_final_report(package_defs, pkg_analysis)
  
  return(invisible(final_result))
}