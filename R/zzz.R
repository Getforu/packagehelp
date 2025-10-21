#' 包加载时执行的初始化函数
#'
#' 检查操作系统是否为支持的平台
#' @param libname 库路径
#' @param pkgname 包名称
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # 检查操作系统，拒绝Linux
  if(Sys.info()["sysname"] == "Linux") {
    stop(
      "\n\n",
      "==========================================\n",
      "本软件包暂不支持Linux系统\n",
      "支持的系统: Windows 和 macOS\n",
      "==========================================\n\n",
      call. = FALSE
    )
  }
}
