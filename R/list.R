
#' List installed packages
#'
#' List installed packages in some library directories.
#'
#' @param filter Regular expression to filter the package list.
#' @param global Whether to list global or local packages.
#' @return An rpkg list of packages.
#' 
#' @export
#' @examples
#' pkg_list()

pkg_list <- function(filter = "", global = FALSE) {
  lib <- pkg_paths(global)
  rds <- get_package_rdss(filter, lib)
  read_package_rds(rds)
}
