#' Check package version and compare with server
#' @param package_name Package name
#' @param server_version Server version from download info
#' @return list with version comparison result
#' @keywords internal
check_package_version <- function(package_name, server_version) {
  # Get locally installed version
  local_version <- tryCatch({
    as.character(packageVersion(package_name))
  }, error = function(e) {
    NULL
  })

  if (is.null(local_version)) {
    # Package not installed
    return(list(
      installed = FALSE,
      need_install = TRUE,
      message = sprintf("准备安装 %s 版本 %s", package_name, server_version)
    ))
  }

  # Compare versions
  local_ver <- package_version(local_version)
  server_ver <- package_version(server_version)

  if (local_ver == server_ver) {
    return(list(
      installed = TRUE,
      need_install = FALSE,
      local_version = local_version,
      server_version = server_version,
      message = sprintf("✓ 已安装最新版本 %s，无需重复安装", local_version)
    ))
  } else if (local_ver > server_ver) {
    return(list(
      installed = TRUE,
      need_install = FALSE,
      local_version = local_version,
      server_version = server_version,
      message = sprintf("✓ 本地版本 %s 高于服务器版本 %s，无需安装", local_version, server_version)
    ))
  } else {
    # local_ver < server_ver - update available
    return(list(
      installed = TRUE,
      need_install = TRUE,
      local_version = local_version,
      server_version = server_version,
      is_update = TRUE,
      message = sprintf("发现新版本 %s (当前: %s)", server_version, local_version)
    ))
  }
}

#' Classify installation error and provide solution
#' @param error_msg Error message
#' @return list with error type, message, and solution
#' @keywords internal
classify_installation_error <- function(error_msg) {
  error_msg_lower <- tolower(as.character(error_msg))

  if (grepl("权限|permission|denied|access", error_msg_lower)) {
    return(list(
      type = "permission",
      message = "安装失败：权限不足",
      solution = paste(
        "建议解决方案：\n",
        "1. 以管理员身份运行R\n",
        "2. 或使用 check_packages() 选择其他安装路径\n",
        "3. 或联系系统管理员获取权限"
      )
    ))
  } else if (grepl("网络|network|timeout|connection|internet|下载", error_msg_lower)) {
    return(list(
      type = "network",
      message = "安装失败：网络连接问题",
      solution = paste(
        "建议解决方案：\n",
        "1. 检查网络连接是否正常\n",
        "2. 检查防火墙设置\n",
        "3. 稍后重试\n",
        "4. 如问题持续，请联系技术支持"
      )
    ))
  } else if (grepl("空间|space|disk|磁盘|容量", error_msg_lower)) {
    return(list(
      type = "disk_space",
      message = "安装失败：磁盘空间不足",
      solution = paste(
        "建议解决方案：\n",
        "1. 清理磁盘空间\n",
        "2. 使用 check_packages() 选择其他磁盘的安装路径\n",
        "3. 删除不需要的文件或程序"
      )
    ))
  } else if (grepl("解压|extract|unzip|untar|corrupt|损坏", error_msg_lower)) {
    return(list(
      type = "extraction",
      message = "安装失败：文件解压错误",
      solution = paste(
        "建议解决方案：\n",
        "1. 重新下载安装包（可能下载不完整）\n",
        "2. 检查磁盘是否有坏道\n",
        "3. 如问题持续，请联系技术支持"
      )
    ))
  } else if (grepl("已存在|exists|占用|in use|locked", error_msg_lower)) {
    return(list(
      type = "file_locked",
      message = "安装失败：文件被占用或已存在",
      solution = paste(
        "建议解决方案：\n",
        "1. 关闭所有使用该包的R会话\n",
        "2. 重启R后重试\n",
        "3. 使用 remove.packages() 手动删除旧版本"
      )
    ))
  } else {
    return(list(
      type = "unknown",
      message = paste("安装失败：", error_msg),
      solution = paste(
        "建议解决方案：\n",
        "1. 检查文件完整性\n",
        "2. 重启R后重试\n",
        "3. 如问题持续，请联系技术支持并提供错误信息"
      )
    ))
  }
}

#' Validate and prepare URL
#' @param url URL
#' @return URL
#' @keywords internal
validate_and_prepare_url <- function(url) {
  if (missing(url) || is.null(url) || url == "") {
    stop("请填写完整地址！", call. = FALSE)
  }

  complete_url <- build_cloud_url(url)
  return(complete_url)
}

#' Detect system environment
#' @return list
#' @keywords internal
detect_system_environment <- function() {
  sys <- Sys.info()[["sysname"]]
  if (sys == "Windows") {
    os_type <- "WIN"
  } else if (sys == "Darwin") {
    os_type <- "MAC"
  } else {
    stop("当前仅支持Windows和macOS系统。", call. = FALSE)
  }

  # Smart extraction function that auto-detects format
  extract_func <- function(file, exdir) {
    file_ext <- tolower(tools::file_ext(file))

    if (file_ext == "zip") {
      # Use unzip for .zip files
      unzip(file, exdir = exdir)
    } else if (file_ext %in% c("tgz", "gz")) {
      # Use untar for .tgz or .tar.gz files
      untar(file, exdir = exdir, tar = "internal")
    } else {
      stop("不支持的压缩格式：", file_ext, call. = FALSE)
    }
  }

  lib_path <- .libPaths()[1]
  cat("安装路径：", lib_path, "\n")

  return(list(
    os_type = os_type,
    extract_func = extract_func,
    lib_path = lib_path
  ))
}

#' Request download permission
#' @param url URL
#' @param machine_code machine
#' @param os_type system type
#' @return list
#' @keywords internal
request_download_permission <- function(url, machine_code, os_type) {
  post_data <- list(
    machine_code = machine_code,
    os_type = os_type
  )

  cat("准备下载...\n")
  res <- tryCatch({
    httr::POST(
      url = url,
      body = jsonlite::toJSON(post_data, auto_unbox = TRUE),
      httr::add_headers("Content-Type" = "application/json"),
      httr::timeout(60)
    )
  }, error = function(e) {
    if (grepl("Timeout", e$message, ignore.case = TRUE)) {
      stop("网络请求超时，请检查网络连接后重试。", call. = FALSE)
    } else {
      stop("网络连接失败，请检查网络设置后重试。", call. = FALSE)
    }
  })

  status_code <- httr::status_code(res)
  if (status_code != 200) {
    if (status_code >= 500) {
      stop("服务器内部错误，请稍后重试。", call. = FALSE)
    } else if (status_code == 404) {
      stop("服务不可用，请检查服务地址。", call. = FALSE)
    }
  }

  text_content <- httr::content(res, "text", encoding = "UTF-8")
  resjson <- tryCatch({
    jsonlite::fromJSON(text_content)
  }, error = function(e) {
    stop("服务器响应格式异常，请稍后重试。", call. = FALSE)
  })

  if (!isTRUE(resjson$success)) {
    if (!is.null(resjson$code)) {
      if (resjson$code == "USER_NOT_REGISTERED") {
        stop("设备未注册，请先完成用户注册。\n机器码：", machine_code, call. = FALSE)
      } else if (resjson$code == "ACCOUNT_DISABLED") {
        stop("账户无法使用。", call. = FALSE)
      } else if (resjson$code == "QUOTA_EXCEEDED") {
        stop("下载受限。", call. = FALSE)
      } else if (resjson$code == "INVALID_OS_TYPE") {
        stop("系统类型参数无效。", call. = FALSE)
      }
    }
    stop("服务器处理异常，请稍后重试。", call. = FALSE)
  }

  return(list(
    package_name = resjson$data$package_name,
    download_url = resjson$data$download_url,
    version = resjson$data$version
  ))
}

#' Download package file
#' @param download_url URL
#' @param os_type type
#' @return path
#' @keywords internal
download_package_file <- function(download_url, os_type) {
  # Auto-detect file extension from URL
  # Supports both .zip and .tgz/.tar.gz formats
  url_ext <- tolower(tools::file_ext(download_url))

  # Determine file extension based on URL
  if (url_ext %in% c("zip")) {
    file_ext <- ".zip"
  } else if (url_ext %in% c("tgz", "gz")) {
    file_ext <- ".tgz"
  } else {
    # Fallback: use platform default if extension unclear
    file_ext <- if (os_type == "MAC") ".tgz" else ".zip"
  }

  temp_file <- tempfile(fileext = file_ext)
  download_success <- FALSE

  # Primary download method using httr
  if (requireNamespace("httr", quietly = TRUE)) {
    cat("正在下载...\n")
    tryCatch({
      response <- httr::GET(
        download_url,
        httr::timeout(120),
        httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
      )

      if (httr::status_code(response) == 200) {
        writeBin(httr::content(response, "raw"), temp_file)
        if (file.exists(temp_file) && file.size(temp_file) > 1000) {
          download_success <- TRUE
          cat("文件下载成功！\n")
        }
      }
    }, error = function(e) {
      cat("下载失败，尝试备用方案...\n")
    })
  }

  if (!download_success) {
    cat("使用备用方案下载...\n")

    if (file.exists(temp_file)) file.remove(temp_file)

    tryCatch({
      old_method <- getOption("download.file.method")
      old_ca_bundle <- Sys.getenv("CURL_CA_BUNDLE")

      options(download.file.method = "libcurl")
      Sys.setenv(CURL_CA_BUNDLE = "")

      utils::download.file(download_url, temp_file, mode = "wb", quiet = TRUE)

      if (!is.null(old_method)) options(download.file.method = old_method)
      Sys.setenv(CURL_CA_BUNDLE = old_ca_bundle)

      if (file.exists(temp_file) && file.size(temp_file) > 1000) {
        download_success <- TRUE
        cat("备用方案下载成功！\n")
      }
    }, error = function(e) {
      if (!is.null(old_method)) options(download.file.method = old_method)
      Sys.setenv(CURL_CA_BUNDLE = old_ca_bundle)
    })
  }

  if (!download_success) {
    if (file.exists(temp_file)) file.remove(temp_file)
    error_msg <- paste0(
      "文件下载失败。请尝试以下解决方案：\n",
      "1. 检查网络连接\n",
      "2. 检查环境\n",
      "3. 重启R后重试\n",
      "如问题持续，请联系技术支持。"
    )
    stop(error_msg, call. = FALSE)
  }

  if (!file.exists(temp_file) || file.size(temp_file) < 1000) {
    if (file.exists(temp_file)) file.remove(temp_file)
    stop("下载文件无效或发生中断，请稍后重试。", call. = FALSE)
  }

  cat("文件下载完成！\n")
  return(temp_file)
}

#' Install and verify package
#' @param temp_file path
#' @param package_name name of the package
#' @param lib_path path
#' @param extract_func extraction function
#' @return TRUE
#' @keywords internal
install_and_verify_package <- function(temp_file, package_name, lib_path, extract_func) {

  # Remove existing package if present
  target_dir <- file.path(lib_path, package_name)
  if (dir.exists(target_dir)) {
    cat("发现现有包，正在删除...\n")
    tryCatch({
      unlink(target_dir, recursive = TRUE, force = TRUE)
      if (dir.exists(target_dir)) {
        stop("无法删除现有包，可能被占用。", call. = FALSE)
      }
      cat("现有包删除成功。\n")
    }, error = function(e) {
      # Classify and display error
      error_info <- classify_installation_error(e$message)
      cat("\n")
      cat(error_info$message, "\n\n")
      cat(error_info$solution, "\n")
      stop(error_info$message, call. = FALSE)
    })
  }

  cat("正在安装包...\n")

  # Direct extraction method (most reliable and compatible)
  tryCatch({
    extract_func(temp_file, exdir = lib_path)

    # Verify the package directory was created
    if (dir.exists(target_dir)) {
      desc_file <- file.path(target_dir, "DESCRIPTION")
      if (file.exists(desc_file)) {
        cat("安装成功！\n")
      } else {
        stop("包结构不完整：缺少DESCRIPTION文件", call. = FALSE)
      }
    } else {
      stop("安装失败：目标目录未创建", call. = FALSE)
    }
  }, error = function(e) {
    # Classify and display error
    error_info <- classify_installation_error(e$message)
    cat("\n")
    cat(error_info$message, "\n\n")
    cat(error_info$solution, "\n")
    stop(error_info$message, call. = FALSE)
  })

  # Final verification - load the package
  tryCatch({
    library(package_name, character.only = TRUE)
    cat("验证成功，可以正常使用。\n")
  }, error = function(e) {
    cat("安装完成，但加载时出现警告。\n")
    cat("这可能是正常的，请尝试重启R后使用。\n")
  })

  return(TRUE)
}

#' Clean up temporary files
#' @param temp_file path
#' @keywords internal
cleanup_temp_files <- function(temp_file) {
  if (file.exists(temp_file)) {
    file.remove(temp_file)
  }
}

#' Install Package or Get Machine Code
#' @param url Character string. Package URL for installation.
#' @param MC Logical. If TRUE, display machine code instead of installing.
#' @param force Logical. If TRUE, force reinstall even if already up-to-date.
#' @param ... Additional arguments (reserved for future use).
#' @return TRUE (invisibly) for installation, or machine code string (invisibly) for MC mode.
#' @export
install_package <- function(url = NULL, MC = FALSE, force = FALSE, ...) {
  # Route to MC handler if MC parameter is TRUE
  if (isTRUE(MC)) {
    # Call internal MC handler module
    return(.mc_handler())
  }

  # Original installation logic
  if (is.null(url) || url == "") {
    stop("请提供必要参数", call. = FALSE)
  }

  # Call internal installation handler
  .install_handler(url, force = force, ...)
}

#' Internal installation handler
#' @keywords internal
.install_handler <- function(url, force = FALSE, ...) {
  complete_url <- validate_and_prepare_url(url)

  sys_env <- detect_system_environment()

  # Use internal MC function to get machine code quietly
  mcode <- .mc_get_quiet()

  download_info <- request_download_permission(complete_url, mcode, sys_env$os_type)

  # Check version before downloading
  version_check <- check_package_version(download_info$package_name, download_info$version)

  cat("\n")
  cat(version_check$message, "\n")

  # If already up-to-date and not forced, skip installation
  if (!version_check$need_install && !force) {
    cat("\n")
    if (version_check$installed) {
      cat("提示：如需重新安装，请使用 install_package(url, force = TRUE)\n")
    }
    return(invisible(FALSE))
  }

  # If update is available, ask for confirmation (optional)
  if (isTRUE(version_check$is_update)) {
    cat(sprintf("\n准备更新 %s: %s → %s\n",
                download_info$package_name,
                version_check$local_version,
                version_check$server_version))
  } else {
    cat(sprintf("\n准备安装 %s 版本 %s\n",
                download_info$package_name,
                download_info$version))
  }

  temp_file <- download_package_file(download_info$download_url, sys_env$os_type)

  install_and_verify_package(temp_file, download_info$package_name,
                           sys_env$lib_path, sys_env$extract_func)

  cleanup_temp_files(temp_file)

  invisible(TRUE)
}

