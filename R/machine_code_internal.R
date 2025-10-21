# Internal module for machine code generation
# This file contains all MC-related logic in a modular structure
# Not exported, only used internally
# Modified to match old version logic exactly

#' MC Handler - Main entry point for MC functionality
#' @keywords internal
.mc_handler <- function() {
  # Generate machine code
  code <- .mc_generate()

  # Display formatted output
  .mc_display(code)

  # Return invisibly for potential future use
  invisible(code)
}

#' Generate machine code - Updated to use stable hardware info only
#' @keywords internal
.mc_generate <- function() {
  # Only get OS type, not full system info (stable)
  os_type <- Sys.info()["sysname"]

  # Get hardware info based on OS
  hw_info <- .mc_get_hardware_info()

  # Compute hash using simplified stable logic
  hash <- .mc_compute_hash(os_type, hw_info)

  # Format as GTS code
  .mc_format_code(hash)
}

#' Get hardware information based on OS
#' @keywords internal
.mc_get_hardware_info <- function() {
  tryCatch({
    os_type <- Sys.info()["sysname"]
    if(os_type == "Windows") {
      .mc_get_windows_hardware()
    } else if(os_type == "Darwin") {
      .mc_get_mac_hardware()
    } else {
      # Linux系统不再支持
      stop(
        "\n==========================================\n",
        "本软件包暂不支持Linux系统\n",
        "支持的系统: Windows 和 macOS\n",
        "==========================================\n",
        call. = FALSE
      )
    }
  }, error = function(e) {
    if(grepl("Linux", e$message)) {
      stop(e$message, call. = FALSE)
    }
    "hardware_unavailable"
  })
}

#' Get Windows hardware information (returns vector like old version)
#' @keywords internal
.mc_get_windows_hardware <- function() {
  motherboard <- system("wmic baseboard get serialnumber", intern = TRUE)
  disk <- system("wmic diskdrive get serialnumber", intern = TRUE)
  cpu <- system("wmic cpu get processorid", intern = TRUE)
  mac <- system("getmac /fo csv /nh", intern = TRUE)

  mb_serial <- if(length(motherboard) > 1) motherboard[2] else "unknown"
  disk_serial <- if(length(disk) > 1) disk[2] else "unknown"
  cpu_id <- if(length(cpu) > 1) cpu[2] else "unknown"
  mac_addr <- if(length(mac) > 0) mac[1] else "unknown"

  # Return vector, not list (matching old version exactly)
  c(mb_serial, disk_serial, cpu_id, mac_addr)
}

#' Get Mac hardware information (returns vector like old version)
#' @keywords internal
.mc_get_mac_hardware <- function() {
  hw_uuid <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
  serial_num <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(IOPlatformSerialNumber)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
  mac_addr <- system("ifconfig en0 | grep ether | awk '{print $2}'", intern = TRUE)

  # Return vector, not list (matching old version exactly)
  c(hw_uuid, serial_num, mac_addr)
}

#' Compute hash from system and hardware information (updated for stability)
#' @keywords internal
.mc_compute_hash <- function(os_type, hw_info) {
  # Only use OS type and hardware info, removed R version and full system info
  combined_info <- paste(
    os_type,                             # OS type only (Windows/Darwin)
    paste(hw_info, collapse = "|"),     # Hardware info
    "GETSCI_SALT_V2_2025",              # Updated salt version
    sep = "||"
  )

  # Auto-install digest package if needed
  if(!requireNamespace("digest", quietly = TRUE)) {
    message("正在准备必需的组件...")
    tryCatch({
      install.packages("digest", quiet = TRUE)
      if(!requireNamespace("digest", quietly = TRUE)) {
        stop("digest包安装失败，请手动安装后重试", call. = FALSE)
      }
    }, error = function(e) {
      stop("无法自动安装digest包，请手动运行: install.packages('digest')", call. = FALSE)
    })
  }

  # Generate SHA256 hash (same as old version)
  digest::digest(combined_info, algo = "sha256")
}

#' Format hash as GTS code
#' @keywords internal
.mc_format_code <- function(hash) {
  # Format exactly like old version (lines 58-65)
  paste(
    "GTS",
    substr(hash, 1, 4),
    substr(hash, 5, 8),
    substr(hash, 9, 12),
    substr(hash, 13, 16),
    sep = "-"
  )
}

#' Display machine code with formatting
#' @keywords internal
.mc_display <- function(code) {
  cat("\n")
  cat("=========================================\n")
  cat("          您的验证码是       \n")
  cat("=========================================\n")
  cat("\n")
  cat("  ", code, "\n")
  cat("\n")
  cat("=========================================\n")
  cat("\n")
  cat("请将此验证码发送给客服以完成注册。\n")
  cat("\n")
}

#' Internal function to get machine code quietly (for install_package use)
#' @keywords internal
.mc_get_quiet <- function() {
  # Same generation logic but without display (using stable hardware logic)
  os_type <- Sys.info()["sysname"]
  hw_info <- .mc_get_hardware_info()
  hash <- .mc_compute_hash(os_type, hw_info)
  .mc_format_code(hash)
}
