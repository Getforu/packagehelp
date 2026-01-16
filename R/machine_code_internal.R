# Internal module for machine code generation
# Simplified version - single hardware identifier per OS
# Windows: MachineGuid (registry)
# macOS: Hardware UUID (ioreg)

#' MC Handler - Main entry point
#' @keywords internal
.mc_handler <- function() {
  code <- .mc_generate()
  .mc_display(code)
  invisible(code)
}

#' Generate machine code
#' @keywords internal
.mc_generate <- function() {
  os_type <- Sys.info()["sysname"]
  hw_id <- .mc_get_hardware_id(os_type)

  if(is.na(hw_id) || nchar(trimws(hw_id)) < 10) {
    stop(
      "\n==========================================\n",
      "错误: 无法获取硬件标识符\n",
      "==========================================\n",
      if(os_type == "Windows") {
        "请尝试以管理员身份运行RStudio\n"
      } else if(os_type == "Darwin") {
        "请在系统弹窗中允许访问硬件信息\n"
      } else {
        "暂不支持此操作系统\n"
      },
      "如仍无法解决，请联系客服协助。\n",
      "==========================================\n",
      call. = FALSE
    )
  }

  hash <- .mc_compute_hash(os_type, hw_id)
  .mc_format_code(os_type, hash)
}

#' Get hardware identifier based on OS
#' @keywords internal
.mc_get_hardware_id <- function(os_type) {
  if(os_type == "Windows") {
    .mc_get_windows_id()
  } else if(os_type == "Darwin") {
    .mc_get_mac_id()
  } else {
    stop(
      "\n==========================================\n",
      "暂不支持此操作系统\n",
      "支持的系统: Windows 和 macOS\n",
      "==========================================\n",
      call. = FALSE
    )
  }
}

#' Get Windows MachineGuid from registry
#' @keywords internal
.mc_get_windows_id <- function() {
  tryCatch({
    result <- system(
      "reg query HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Cryptography /v MachineGuid",
      intern = TRUE,
      ignore.stderr = TRUE
    )
    if(length(result) > 0) {
      guid_line <- result[grepl("MachineGuid", result)]
      if(length(guid_line) > 0) {
        guid_match <- regmatches(guid_line, regexpr("[A-Fa-f0-9-]{36}", guid_line))
        if(length(guid_match) > 0) {
          return(guid_match[1])
        }
      }
    }
    NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)
}

#' Get macOS Hardware UUID
#' @keywords internal
.mc_get_mac_id <- function() {
  tryCatch({
    result <- system(
      "ioreg -rd1 -c IOPlatformExpertDevice | awk '/IOPlatformUUID/{print $3}' | tr -d '\"'",
      intern = TRUE,
      ignore.stderr = TRUE
    )
    if(length(result) > 0 && nchar(trimws(result[1])) > 10) {
      return(trimws(result[1]))
    }
    NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)
}

#' Compute hash from hardware identifier
#' @keywords internal
.mc_compute_hash <- function(os_type, hw_id) {
  combined_info <- paste(
    os_type,
    hw_id,
    "GETSCI_SALT_V3_2026",
    sep = "||"
  )

  if(!requireNamespace("digest", quietly = TRUE)) {
    message("正在准备必要组件...")
    tryCatch({
      install.packages("digest", quiet = TRUE)
      if(!requireNamespace("digest", quietly = TRUE)) {
        stop("无法安装digest包", call. = FALSE)
      }
    }, error = function(e) {
      stop("无法安装digest包", call. = FALSE)
    })
  }

  digest::digest(combined_info, algo = "sha256")
}

#' Format hash as machine code with OS-specific prefix
#' @keywords internal
.mc_format_code <- function(os_type, hash) {
  prefix <- if(os_type == "Windows") "GTSW" else if(os_type == "Darwin") "GTSM" else "GTSX"

  paste(
    prefix,
    substr(hash, 1, 4),
    substr(hash, 5, 8),
    substr(hash, 9, 12),
    substr(hash, 13, 16),
    sep = "-"
  )
}

#' Display machine code
#' @keywords internal
.mc_display <- function(code) {
  cat("\n")
  cat("=========================================\n")
  cat("           您的验证码\n")
  cat("=========================================\n")
  cat("\n")
  cat("  ", code, "\n")
  cat("\n")
  cat("=========================================\n")
  cat("\n")
  cat("请将此验证码发送给客服激活授权。\n")
  cat("\n")
}

#' Get machine code quietly (no display)
#' @keywords internal
.mc_get_quiet <- function() {
  os_type <- Sys.info()["sysname"]
  hw_id <- .mc_get_hardware_id(os_type)

  if(is.na(hw_id) || nchar(trimws(hw_id)) < 10) {
    stop("无法获取硬件标识符", call. = FALSE)
  }

  hash <- .mc_compute_hash(os_type, hw_id)
  .mc_format_code(os_type, hash)
}
