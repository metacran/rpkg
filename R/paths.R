
pkg_paths <- function(global) {
  if (global) .libPaths() else pkg_local_path()
}

.onLoad <- function(libname, pkgname) {
  path <- pkg_local_path()
  .libPaths(c(path, .libPaths()))
}

pkg_local_path <- function() {
  file.path(getwd(), "r_pkgs")
}
