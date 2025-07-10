# URL processing utility functions

#' Detect input type
#' @param input user input string
#' @return string
#' @keywords internal
detect_input_type <- function(input) {
  if (is.null(input) || !is.character(input) || length(input) != 1 || nchar(input) == 0) {
    return("invalid")
  }

  if (grepl("^https?://", input, ignore.case = TRUE)) {
    return("full_url")
  }

  if (grepl("^[0-9]+-[a-zA-Z0-9]+$", input)) {
    return("simple_code")
  }

  return("invalid")
}

#' Validate URL input format
#' @param input user input
#' @return logical value
#' @keywords internal
validate_url_input <- function(input) {
  input_type <- detect_input_type(input)

  if (input_type == "invalid") {
    return(FALSE)
  }

  if (input_type == "full_url") {
    pattern <- "^https://[0-9]+-[a-zA-Z0-9]+\\.ap-[a-z]+\\.tencentscf\\.com/?$"
    return(grepl(pattern, input))
  }

  if (input_type == "simple_code") {
    return(TRUE)
  }
  return(FALSE)
}

#' Build cloud function URL
#' @param input user input
#' @param region cloud function region
#' @return complete URL
#' @keywords internal
build_cloud_url <- function(input, region = "ap-shanghai") {
  if (!validate_url_input(input)) {
    stop("URL格式无效。", call. = FALSE)
  }
  input_type <- detect_input_type(input)

  if (input_type == "full_url") {
    return(gsub("/$", "", input))
  }

  if (input_type == "simple_code") {
    return(paste0("https://", input, ".", region, ".tencentscf.com"))
  }

  stop("URL处理异常", call. = FALSE)
}
