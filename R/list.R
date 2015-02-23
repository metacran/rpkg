
#' List installed packages
#'
#' List installed packages in some library directories.
#'
#' @param lib Character vector of library directories,
#'   defaults to \code{\link{pkg_paths}}.
#' @return An rpkg list of packages.
#' 
#' @export
#' @examples
#' pkg_list()

pkg_list <- function(lib = pkg_paths()) {
  lib <- as.character(lib)
  dirs <- unlist(lapply(lib, dir, full.names = TRUE))
  rds_files <- drop_non_existant(file.path(dirs, "Meta", "package.rds"))
  read_package_rds(rds_files)
}
