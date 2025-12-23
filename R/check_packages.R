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
    install_optional_packages(package_defs$optional_packages, interactive)
  }

  final_result <- generate_final_report(package_defs, pkg_analysis)

  return(invisible(final_result))
}
