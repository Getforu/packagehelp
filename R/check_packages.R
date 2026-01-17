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
        critical_version_mismatch = pkg_analysis$critical_version_mismatch,
        status = "cancelled"
      )))
    }
    if (!is.null(install_result$missing_essential)) {
      pkg_analysis$missing_essential <- install_result$missing_essential
    }
  }

  if (interactive && length(pkg_analysis$missing_essential) == 0) {
    install_optional_packages(package_defs$optional_packages, package_defs$special_packages, interactive = interactive)
  }

  final_result <- generate_final_report(package_defs, pkg_analysis)

  return(invisible(final_result))
}

#' Check special packages that cannot be installed from CRAN
#' @param special_packages list of special packages
#' @keywords internal
check_special_packages <- function(special_packages) {
  if (length(special_packages) == 0) return(invisible(NULL))

  missing_special <- c()
  installed_special <- c()

  for (pkg_name in names(special_packages)) {
    if (requireNamespace(pkg_name, quietly = TRUE)) {
      installed_special <- c(installed_special, pkg_name)
    } else {
      missing_special <- c(missing_special, pkg_name)
    }
  }

  if (length(missing_special) > 0) {
    cat("\n")
    cat("===========================================\n")
    cat("        特殊安装包检查\n")
    cat("===========================================\n")
    cat("\n")
    cat(sprintf("检测到 %d 个特殊包未安装:\n", length(missing_special)))
    cat("这些包无法从CRAN直接安装，需要手动处理。\n\n")

    for (pkg_name in missing_special) {
      pkg_info <- special_packages[[pkg_name]]
      cat(sprintf("【%s】%s\n", pkg_name, pkg_info$description))
      cat(sprintf("  推荐版本: %s\n", pkg_info$version))
      cat(sprintf("  安装指南: %s\n", pkg_info$install_guide))
      cat("\n")
    }

    cat("-------------------------------------------\n")
    cat("提示: 如需使用这些包的相关功能，请按照上述指南手动安装，\n")
    cat("      或联系客服获取帮助。\n")
    cat("===========================================\n")
  }

  if (length(installed_special) > 0) {
    cat(sprintf("\n已安装的特殊包: %s\n", paste(installed_special, collapse = ", ")))
  }

  invisible(list(
    installed = installed_special,
    missing = missing_special
  ))
}
