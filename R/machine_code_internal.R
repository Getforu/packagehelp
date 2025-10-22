# Internal module for machine code generation
# This file contains all MC-related logic in a modular structure
# Not exported, only used internally
# Updated to use enhanced machine code generation with UUID and computer/user info

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

#' Generate machine code - Enhanced with UUID and system identifiers
#' @keywords internal
.mc_generate <- function() {
  # Get OS type
  os_type <- Sys.info()["sysname"]

  # Get computer name and user name (cross-platform stable)
  computer_name <- Sys.info()["nodename"]
  user_name <- Sys.info()["user"]

  # Get or create persistent UUID
  persistent_uuid <- .mc_get_or_create_uuid()

  # Get hardware info based on OS
  hw_info <- .mc_get_hardware_info()

  # Check if hardware info failed and show warning if needed
  .mc_check_hardware_warning(os_type, hw_info, quiet = FALSE)

  # Compute hash using enhanced stable logic
  hash <- .mc_compute_hash(os_type, computer_name, user_name, persistent_uuid, hw_info)

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

#' Get Windows hardware information (returns vector)
#' @keywords internal
.mc_get_windows_hardware <- function() {
  motherboard <- system("wmic baseboard get serialnumber", intern = TRUE)
  disk <- system("wmic diskdrive get serialnumber", intern = TRUE)
  cpu <- system("wmic cpu get processorid", intern = TRUE)
  mac <- system("getmac /fo csv /nh", intern = TRUE)

  mb_serial <- if(length(motherboard) > 1) trimws(motherboard[2]) else "unknown"
  disk_serial <- if(length(disk) > 1) trimws(disk[2]) else "unknown"
  cpu_id <- if(length(cpu) > 1) trimws(cpu[2]) else "unknown"
  mac_addr <- if(length(mac) > 0) mac[1] else "unknown"

  c(mb_serial, disk_serial, cpu_id, mac_addr)
}

#' Get Mac hardware information (returns vector)
#' @keywords internal
.mc_get_mac_hardware <- function() {
  hw_uuid <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
  serial_num <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(IOPlatformSerialNumber)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
  mac_addr <- system("ifconfig en0 | grep ether | awk '{print $2}'", intern = TRUE)

  c(hw_uuid, serial_num, mac_addr)
}

#' Check hardware info and stop if failed
#' @keywords internal
.mc_check_hardware_warning <- function(os_type, hw_info, quiet = FALSE) {
  if(all(grepl("unknown", hw_info)) || identical(hw_info, "hardware_unavailable")) {
    stop(
      "\n==========================================\n",
      "错误: 无法获取硬件信息\n",
      "==========================================\n",
      if(os_type == "Windows") {
        "解决方法:\n1. 请右键以管理员模式运行RStudio\n2. 确保有足够的系统权限\n"
      } else if(os_type == "Darwin") {
        "解决方法:\n1. 请在系统弹窗中允许访问硬件信息\n2. 或在「系统偏好设置 > 安全性与隐私」中授权\n"
      } else {
        ""
      },
      "\n如仍无法解决，请联系客服协助。\n",
      "==========================================\n",
      call. = FALSE
    )
  }
}

#' Compute hash from all system information (updated for enhanced uniqueness)
#' @keywords internal
.mc_compute_hash <- function(os_type, computer_name, user_name, persistent_uuid, hw_info) {
  # Combine OS type, computer name, user name, UUID, and hardware info
  combined_info <- paste(
    os_type,
    computer_name,
    user_name,
    persistent_uuid,
    paste(hw_info, collapse = "|"),
    "GETSCI_SALT_V3_2025",
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

  # Generate SHA256 hash
  digest::digest(combined_info, algo = "sha256")
}

#' Format hash as GTS code
#' @keywords internal
.mc_format_code <- function(hash) {
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
  # Same generation logic but without display
  os_type <- Sys.info()["sysname"]
  computer_name <- Sys.info()["nodename"]
  user_name <- Sys.info()["user"]
  persistent_uuid <- .mc_get_or_create_uuid()
  hw_info <- .mc_get_hardware_info()
  # No warning in quiet mode
  hash <- .mc_compute_hash(os_type, computer_name, user_name, persistent_uuid, hw_info)
  .mc_format_code(hash)
}

#' Get or create persistent UUID
#' @keywords internal
.mc_get_or_create_uuid <- function() {
  # Determine UUID file path (cross-platform)
  home_dir <- Sys.getenv("HOME")
  if(home_dir == "" || !dir.exists(home_dir)) {
    home_dir <- Sys.getenv("USERPROFILE")  # Windows fallback
  }

  uuid_file <- file.path(home_dir, ".getsci_uuid")

  # If UUID file exists, read it
  if(file.exists(uuid_file)) {
    tryCatch({
      uuid <- readLines(uuid_file, n = 1, warn = FALSE)
      if(length(uuid) > 0 && nchar(uuid) > 0) {
        return(uuid)
      }
    }, error = function(e) {
      # Read failed, continue to generate new UUID
    })
  }

  # Generate new UUID and save
  # Auto-install digest if needed (for UUID generation)
  if(!requireNamespace("digest", quietly = TRUE)) {
    tryCatch({
      install.packages("digest", quiet = TRUE)
    }, error = function(e) {
      # Installation failed, UUID will be less unique but still usable
    })
  }

  new_uuid <- paste(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    sample(10000:99999, 1),
    if(requireNamespace("digest", quietly = TRUE)) {
      digest::digest(paste(Sys.info(), collapse = ""), algo = "md5")
    } else {
      paste(sample(c(0:9, letters), 32, replace = TRUE), collapse = "")
    },
    sep = "-"
  )

  tryCatch({
    writeLines(new_uuid, uuid_file)
  }, error = function(e) {
    # Save failed, doesn't affect usage, just regenerates next time
  })

  return(new_uuid)
}
