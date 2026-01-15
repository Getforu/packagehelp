# Internal module for machine code generation
# Not exported, only used internally

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
  persistent_uuid <- .mc_get_or_create_uuid()
  hw_info <- .mc_get_hardware_info()

  .mc_check_hardware_warning(os_type, hw_info, quiet = FALSE)
  hash <- .mc_compute_hash(os_type, persistent_uuid, hw_info)
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
      stop(
        "\n==========================================\n",
        "Linux is not supported.\n",
        "Supported: Windows and macOS\n",
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

#' Get Windows hardware information
#' @keywords internal
.mc_get_windows_hardware <- function() {
  invalid_values <- c("", "unknown", "to be filled by o.e.m.", "default string",
                      "none", "n/a", "system serial number", "0", "123456789",
                      "not available", "chassis serial number", "base board serial number")

  is_valid_hw <- function(val) {
    if(is.null(val) || length(val) == 0) return(FALSE)
    val_clean <- tolower(trimws(val))
    if(nchar(val_clean) < 3) return(FALSE)
    if(val_clean %in% invalid_values) return(FALSE)
    TRUE
  }

  # MachineGuid from registry
  machine_guid <- tryCatch({
    result <- system("reg query HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Cryptography /v MachineGuid",
                     intern = TRUE, ignore.stderr = TRUE)
    result
  }, error = function(e) NULL, warning = function(w) NULL)

  guid_val <- if(length(machine_guid) > 0) {
    guid_line <- machine_guid[grepl("MachineGuid", machine_guid)]
    if(length(guid_line) > 0) {
      guid_match <- regmatches(guid_line, regexpr("[A-Fa-f0-9-]{36}", guid_line))
      if(length(guid_match) > 0 && is_valid_hw(guid_match[1])) guid_match[1] else NA_character_
    } else NA_character_
  } else NA_character_

  get_wmic_value <- function(cmd, row = 2) {
    tryCatch({
      result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
      if(length(result) >= row) {
        val <- trimws(result[row])
        if(is_valid_hw(val)) val else NA_character_
      } else NA_character_
    }, error = function(e) NA_character_, warning = function(w) NA_character_)
  }

  mb_serial <- get_wmic_value("wmic baseboard get serialnumber")
  cpu_id <- get_wmic_value("wmic cpu get processorid")

  # MAC address: filter virtual NICs, sorted
  mac_addr <- tryCatch({
    result <- system("getmac /fo csv /nh", intern = TRUE, ignore.stderr = TRUE)
    if(length(result) > 0) {
      virtual_mac_prefixes <- c(
        "00-50-56", "00-0C-29", "00-05-69",
        "08-00-27",
        "00-15-5D",
        "00-03-FF",
        "00-1C-42",
        "02-00-4C", "02-42-"
      )

      all_macs <- c()
      for(line in result) {
        mac_match <- regmatches(line, regexpr("[0-9A-Fa-f]{2}(-[0-9A-Fa-f]{2}){5}", line))
        if(length(mac_match) > 0 && is_valid_hw(mac_match[1])) {
          all_macs <- c(all_macs, mac_match[1])
        }
      }

      is_virtual <- function(mac) {
        mac_upper <- toupper(mac)
        for(prefix in virtual_mac_prefixes) {
          if(startsWith(mac_upper, toupper(prefix))) return(TRUE)
        }
        FALSE
      }

      physical_macs <- all_macs[!sapply(all_macs, is_virtual)]

      if(length(physical_macs) > 0) {
        sort(physical_macs)[1]
      } else if(length(all_macs) > 0) {
        sort(all_macs)[1]
      } else {
        NA_character_
      }
    } else NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)

  c(mb_serial, cpu_id, mac_addr, guid_val)
}

#' Get Mac hardware information
#' @keywords internal
.mc_get_mac_hardware <- function() {
  is_valid_hw <- function(val) {
    if(is.null(val) || length(val) == 0) return(FALSE)
    val_clean <- trimws(val)
    if(nchar(val_clean) < 3) return(FALSE)
    TRUE
  }

  # Hardware UUID
  hw_uuid <- tryCatch({
    result <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'",
                     intern = TRUE, ignore.stderr = TRUE)
    if(length(result) > 0 && is_valid_hw(result[1])) result[1] else NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)

  # Serial number
  serial_num <- tryCatch({
    result <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(IOPlatformSerialNumber)' | awk '{print $3}' | sed 's/\"//g'",
                     intern = TRUE, ignore.stderr = TRUE)
    if(length(result) > 0 && is_valid_hw(result[1])) result[1] else NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)

  # MAC address: try multiple interfaces (en0, en1, en2)
  mac_addr <- tryCatch({
    interfaces <- c("en0", "en1", "en2")
    found_mac <- NA_character_

    for(iface in interfaces) {
      result <- tryCatch({
        system(paste0("ifconfig ", iface, " | grep ether | awk '{print $2}'"),
               intern = TRUE, ignore.stderr = TRUE)
      }, error = function(e) character(0))

      if(length(result) > 0 && is_valid_hw(result[1])) {
        found_mac <- result[1]
        break
      }
    }
    found_mac
  }, error = function(e) NA_character_, warning = function(w) NA_character_)

  c(hw_uuid, serial_num, mac_addr)
}

#' Diagnose hardware information retrieval
#' @keywords internal
.mc_diagnose <- function() {
  os_type <- Sys.info()["sysname"]

  cat("\n")
  cat("==========================================\n")
  cat("        Hardware Diagnostic Report\n")
  cat("==========================================\n")
  cat("\n")
  cat("OS:       ", os_type, "\n")
  cat("Computer: ", Sys.info()["nodename"], "\n")
  cat("User:     ", Sys.info()["user"], "\n")
  cat("\n")
  cat("------------------------------------------\n")
  cat("Hardware Identifiers:\n")
  cat("------------------------------------------\n")

  format_status <- function(name, value, is_valid) {
    status <- if(is_valid) "[OK]" else "[FAIL]"
    val_display <- if(is_valid) {
      if(nchar(value) > 12) {
        paste0(substr(value, 1, 4), "****", substr(value, nchar(value)-3, nchar(value)))
      } else {
        value
      }
    } else {
      if(is.na(value)) "Not retrieved" else value
    }
    cat(sprintf("  %s %s: %s\n", status, name, val_display))
  }

  is_valid_hw <- function(val) {
    !is.na(val) && nchar(trimws(val)) >= 3 && !grepl("^unknown$", val, ignore.case = TRUE)
  }

  success_count <- 0

  if(os_type == "Windows") {
    cat("\n")

    guid <- tryCatch({
      result <- system("reg query HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Cryptography /v MachineGuid",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) {
        guid_line <- result[grepl("MachineGuid", result)]
        if(length(guid_line) > 0) {
          guid_match <- regmatches(guid_line, regexpr("[A-Fa-f0-9-]{36}", guid_line))
          if(length(guid_match) > 0) guid_match[1] else NA_character_
        } else NA_character_
      } else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(guid)
    if(valid) success_count <- success_count + 1
    format_status("MachineGuid", guid, valid)

    mb <- tryCatch({
      result <- system("wmic baseboard get serialnumber", intern = TRUE, ignore.stderr = TRUE)
      if(length(result) >= 2) trimws(result[2]) else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(mb)
    if(valid) success_count <- success_count + 1
    format_status("Motherboard Serial", mb, valid)

    cpu <- tryCatch({
      result <- system("wmic cpu get processorid", intern = TRUE, ignore.stderr = TRUE)
      if(length(result) >= 2) trimws(result[2]) else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(cpu)
    if(valid) success_count <- success_count + 1
    format_status("CPU ID", cpu, valid)

    mac <- tryCatch({
      result <- system("getmac /fo csv /nh", intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) {
        mac_match <- regmatches(result[1], regexpr("[0-9A-Fa-f]{2}(-[0-9A-Fa-f]{2}){5}", result[1]))
        if(length(mac_match) > 0) mac_match[1] else NA_character_
      } else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(mac)
    if(valid) success_count <- success_count + 1
    format_status("MAC Address", mac, valid)

  } else if(os_type == "Darwin") {
    cat("\n")

    hw_uuid <- tryCatch({
      result <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) result[1] else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(hw_uuid)
    if(valid) success_count <- success_count + 1
    format_status("Hardware UUID", hw_uuid, valid)

    serial <- tryCatch({
      result <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(IOPlatformSerialNumber)' | awk '{print $3}' | sed 's/\"//g'",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) result[1] else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(serial)
    if(valid) success_count <- success_count + 1
    format_status("Serial Number", serial, valid)

    mac <- tryCatch({
      result <- system("ifconfig en0 | grep ether | awk '{print $2}'",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) result[1] else NA_character_
    }, error = function(e) NA_character_)
    valid <- is_valid_hw(mac)
    if(valid) success_count <- success_count + 1
    format_status("MAC Address", mac, valid)
  }

  cat("\n")
  cat("------------------------------------------\n")
  cat(sprintf("Total: %d valid (minimum 2 required)\n", success_count))
  cat("------------------------------------------\n")

  if(success_count < 2) {
    cat("\n")
    cat("Warning: Insufficient hardware info\n")
    cat("\n")
    if(os_type == "Windows") {
      cat("Suggestions:\n")
      cat("  1. Run RStudio as Administrator\n")
      cat("  2. Check if security software blocks wmic\n")
      cat("  3. Test manually in PowerShell:\n")
      cat("     wmic baseboard get serialnumber\n")
    } else if(os_type == "Darwin") {
      cat("Suggestions:\n")
      cat("  1. Allow terminal access in system popup\n")
      cat("  2. Check Security & Privacy settings\n")
      cat("  3. Test manually in Terminal:\n")
      cat("     ioreg -rd1 -c IOPlatformExpertDevice\n")
    }
  } else {
    cat("\n")
    cat("OK: Hardware info sufficient\n")
  }

  cat("\n")
  cat("==========================================\n")
  cat("\n")

  invisible(list(
    os_type = os_type,
    success_count = success_count,
    min_required = 2
  ))
}

#' Check hardware info and stop if insufficient
#' @keywords internal
.mc_check_hardware_warning <- function(os_type, hw_info, quiet = FALSE) {
  success_count <- sum(
    !is.na(hw_info) &
    hw_info != "hardware_unavailable" &
    !grepl("^unknown$", hw_info, ignore.case = TRUE) &
    nchar(trimws(hw_info)) >= 3
  )

  min_required <- 2

  if(success_count < min_required) {
    if(!quiet) {
      .mc_diagnose()
    }

    stop(
      "\nError: Insufficient hardware info\n",
      sprintf("Retrieved: %d, Required: at least %d\n", success_count, min_required),
      "\nPlease contact support for assistance.\n",
      call. = FALSE
    )
  }
}

#' Compute hash from system information
#' @keywords internal
.mc_compute_hash <- function(os_type, persistent_uuid, hw_info) {
  valid_hw_info <- hw_info[!is.na(hw_info) & nchar(trimws(hw_info)) >= 3]
  valid_hw_info <- sort(valid_hw_info)

  combined_info <- paste(
    os_type,
    persistent_uuid,
    paste(valid_hw_info, collapse = "|"),
    "GETSCI_SALT_V3_2025",
    sep = "||"
  )

  if(!requireNamespace("digest", quietly = TRUE)) {
    message("Preparing required components...")
    tryCatch({
      install.packages("digest", quiet = TRUE)
      if(!requireNamespace("digest", quietly = TRUE)) {
        stop("Failed to install digest package", call. = FALSE)
      }
    }, error = function(e) {
      stop("Cannot install digest package. Run: install.packages('digest')", call. = FALSE)
    })
  }

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

#' Display machine code
#' @keywords internal
.mc_display <- function(code) {
  cat("\n")
  cat("=========================================\n")
  cat("        Your Verification Code\n")
  cat("=========================================\n")
  cat("\n")
  cat("  ", code, "\n")
  cat("\n")
  cat("=========================================\n")
  cat("\n")
  cat("Please send this code to support.\n")
  cat("\n")
}

#' Get machine code quietly
#' @keywords internal
.mc_get_quiet <- function() {
  os_type <- Sys.info()["sysname"]
  persistent_uuid <- .mc_get_or_create_uuid()
  hw_info <- .mc_get_hardware_info()

  .mc_check_hardware_warning(os_type, hw_info, quiet = TRUE)

  hash <- .mc_compute_hash(os_type, persistent_uuid, hw_info)
  .mc_format_code(hash)
}

#' Compute hardware fingerprint for UUID binding
#' @keywords internal
.mc_compute_hw_fingerprint <- function() {
  os_type <- Sys.info()["sysname"]

  core_id <- if(os_type == "Windows") {
    tryCatch({
      result <- system("reg query HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Cryptography /v MachineGuid",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0) {
        guid_line <- result[grepl("MachineGuid", result)]
        if(length(guid_line) > 0) {
          guid_match <- regmatches(guid_line, regexpr("[A-Fa-f0-9-]{36}", guid_line))
          if(length(guid_match) > 0) guid_match[1] else ""
        } else ""
      } else ""
    }, error = function(e) "")
  } else if(os_type == "Darwin") {
    tryCatch({
      result <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'",
                       intern = TRUE, ignore.stderr = TRUE)
      if(length(result) > 0 && nchar(trimws(result[1])) > 10) result[1] else ""
    }, error = function(e) "")
  } else {
    ""
  }

  combined <- paste(os_type, core_id, "GETSCI_FP_SALT", sep = "||")

  if(requireNamespace("digest", quietly = TRUE)) {
    digest::digest(combined, algo = "md5")
  } else {
    paste(charToRaw(substr(combined, 1, 32)), collapse = "")
  }
}

#' Get or create persistent UUID with fingerprint binding
#' @keywords internal
.mc_get_or_create_uuid <- function() {
  home_dir <- Sys.getenv("HOME")
  if(home_dir == "" || !dir.exists(home_dir)) {
    home_dir <- Sys.getenv("USERPROFILE")
  }

  uuid_file <- file.path(home_dir, ".getsci_uuid")
  current_fingerprint <- .mc_compute_hw_fingerprint()

  if(file.exists(uuid_file)) {
    tryCatch({
      content <- readLines(uuid_file, n = 1, warn = FALSE)
      if(length(content) > 0 && nchar(content) > 0) {
        parts <- strsplit(content, "\\|\\|FP\\|\\|", fixed = FALSE)[[1]]

        if(length(parts) == 2) {
          stored_uuid <- parts[1]
          stored_fingerprint <- parts[2]

          if(stored_fingerprint == current_fingerprint) {
            return(stored_uuid)
          } else {
            message("Hardware change detected, regenerating identifier...")
          }
        } else if(length(parts) == 1 && nchar(parts[1]) > 20) {
          legacy_uuid <- parts[1]
          new_content <- paste0(legacy_uuid, "||FP||", current_fingerprint)
          tryCatch({
            writeLines(new_content, uuid_file)
          }, error = function(e) {})
          return(legacy_uuid)
        }
      }
    }, error = function(e) {
    })
  }

  if(!requireNamespace("digest", quietly = TRUE)) {
    tryCatch({
      install.packages("digest", quiet = TRUE)
    }, error = function(e) {
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

  uuid_with_fingerprint <- paste0(new_uuid, "||FP||", current_fingerprint)

  tryCatch({
    writeLines(uuid_with_fingerprint, uuid_file)
  }, error = function(e) {
  })

  return(new_uuid)
}
