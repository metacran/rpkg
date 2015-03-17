
#' List installed packages
#'
#' List installed packages in some library directories.
#'
#' @param filter Regular expression to filter the package list.
#' @param lib Character vector of library directories,
#'   defaults to \code{\link{pkg_paths}}.
#' @return An rpkg list of packages.
#' 
#' @export
#' @examples
#' pkg_list()

pkg_list <- function(filter = "", lib = pkg_paths()) {
  rds <- get_package_rdss(filter, lib)
  read_package_rds(rds)
}
