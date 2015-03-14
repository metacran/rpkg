
#' Dependency tree of a package
#'
#' @param pkg Package name, optionally with version number after a dash.
#' @param include_base Whether to include base R packages in the tree.
#'
#' @export

pkg_tree <- function(pkg, include_base = FALSE) {
  draw_tree(pkg_deps(pkg, include_base = include_base))
}

draw_tree <- function(deps, prefix = "", include_base = FALSE) {

  if (prefix != "") cat(prefix, "`--", sep = "")
  cat(deps$package, "\n")
  for (el in deps$deps) {
    if (length(el)) draw_tree(el, prefix = paste0("   ", prefix))
  }

  invisible()
}
