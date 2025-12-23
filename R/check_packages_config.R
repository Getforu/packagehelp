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

#' Get package definitions
#' @return list
#' @keywords internal
get_package_definitions <- function() {
  min_r_version <- "4.0.0"
  base_packages <- c("base", "graphics", "grDevices", "grid", "stats", "utils","tools")

  essential_packages <- list(
    "remotes" = "2.4.2",
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
    "readxl" = "1.4.3",
    "arrow" = "20.0.0.2",
    "feather" = "0.3.5",
    "haven" = "2.5.3",
    "gt" = "0.9.0",
    "magrittr" = "2.0.3",
    "foreign" = "0.8-86",
    "sas7bdat" = "0.8",
    "mice" = "3.16.0",
    "openxlsx" = "4.2.5.2",
    "pROC" = "1.18.5",
    "rlang" = "1.1.1",
    "rms" = "6.7-1",
    "data.table" = "1.14.8",
    "rjson" = "0.2.21",
    "readr" = "2.1.4",
    "vroom" = "1.6.3",
    "survival" = "3.6-4",
    "tibble" = "3.2.1",
    "VIM" = "6.2.2",
    "stringr" = "1.5.0",
    "lubridate" = "1.9.2"
  )

  optional_packages <- list(
    "机器学习" = list(
      "xgboost" = "1.7.8.1",
      "caret" = "6.0.94",
      "glmnet" = "4.1.8",
      "randomForest" = "4.7.1.1",
      "gbm" = "2.2.2",
      "lightgbm" = "4.6.0",
      "catboost" = "1.2.7",
      "rpart" = "4.1.23",
      "adabag" = "5.0",
      "ranger" = "0.15.1",
      "e1071" = "1.7.13",
      "nnet" = "7.3.19",
      "kknn" = "1.4.1",
      "ggsci" = "3.0.0",
      "viridis" = "0.6.4",
      "reshape2" = "1.4.4",
      "ggradar" = "0.2",
      "ggrepel" = "0.9.3",
      "kernelshap" = "0.7.0",
      "shapviz" = "0.9.7"
    ),
    "高级绘图" = list(
      "patchwork" = "1.1.2"
    ),
    "预测模型" = list(
      "MASS" = "7.3-60.2",
      "Hmisc" = "5.1-0",
      "Boruta" = "8.0.0",
      "smotefamily" = "1.4.0",
      "dcurves" = "0.5.0",
      "rmda" = "1.6",
      "ResourceSelection" = "0.3-6",
      "classInt" = "0.4-11",
      "corrplot" = "0.92",
      "scales" = "1.3.0",
      "htmltools" = "0.5.5",
      "writexl" = "1.4.2",
      "DynNom" = "5.1",
      "shiny" = "1.7.5",
      "bslib" = "0.5.1",
      "shinyWidgets" = "0.9.0",
      "plotly" = "4.10.2"
    )
  )

  return(list(
    min_r_version = min_r_version,
    base_packages = base_packages,
    essential_packages = essential_packages,
    optional_packages = optional_packages
  ))
}
