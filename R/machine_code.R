#' Generate machine code
#'
#' Generate unique machine identifier
#'
#' @param quiet suppress output
#' @return formatted machine code
#' @keywords internal
machine_code <- function(quiet = FALSE) {
  if(!requireNamespace("digest", quietly = TRUE)) {
    install.packages("digest")
  }

  os_info <- Sys.info()
  r_version <- R.version.string

  hw_info <- tryCatch({
    if(Sys.info()["sysname"] == "Windows") {
      motherboard <- system("wmic baseboard get serialnumber", intern = TRUE)
      disk <- system("wmic diskdrive get serialnumber", intern = TRUE)
      cpu <- system("wmic cpu get processorid", intern = TRUE)
      mac <- system("getmac /fo csv /nh", intern = TRUE)

      mb_serial <- if(length(motherboard) > 1) motherboard[2] else "unknown"
      disk_serial <- if(length(disk) > 1) disk[2] else "unknown"
      cpu_id <- if(length(cpu) > 1) cpu[2] else "unknown"
      mac_addr <- if(length(mac) > 0) mac[1] else "unknown"

      c(mb_serial, disk_serial, cpu_id, mac_addr)
    } else if(Sys.info()["sysname"] == "Darwin") {
      hw_uuid <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(UUID)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
      serial_num <- system("ioreg -rd1 -c IOPlatformExpertDevice | grep -E '(IOPlatformSerialNumber)' | awk '{print $3}' | sed 's/\"//g'", intern = TRUE)
      mac_addr <- system("ifconfig en0 | grep ether | awk '{print $2}'", intern = TRUE)

      c(hw_uuid, serial_num, mac_addr)
    } else {
      hostname <- tryCatch(system("hostname -f", intern = TRUE), error = function(e) "unknown")
      machine_id <- tryCatch(system("cat /etc/machine-id 2>/dev/null || echo unknown", intern = TRUE), error = function(e) "unknown")
      ip_addr <- tryCatch(system("hostname -I | head -n1 | awk '{print $1}'", intern = TRUE), error = function(e) "unknown")

      user_env <- paste(Sys.getenv("USER"), Sys.getenv("HOME"), paste(.libPaths(), collapse=":"), sep="|")

      c(hostname, machine_id, ip_addr, user_env)
    }
  }, error = function(e) {
    "hardware_unavailable"
  })

  combined_info <- paste(
    paste(os_info, collapse = "|"),
    paste(hw_info, collapse = "|"),
    r_version,
    "GETSCI_SALT_83921",
    sep = "||"
  )

  hash <- digest::digest(combined_info, algo = "sha256")

  formatted_code <- paste(
    "GTS",
    substr(hash, 1, 4),
    substr(hash, 5, 8),
    substr(hash, 9, 12),
    substr(hash, 13, 16),
    sep = "-"
  )

  if(!quiet) {
    cat("\n您的机器码是: ", formatted_code, "\n")
    cat("请提交此机器码激活授权。\n\n")
  }

  return(formatted_code)
}
