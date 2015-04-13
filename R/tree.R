
#' Dependency tree of a package
#'
#' @param pkg Package name, optionally with version number after a dash.
#' @param include_base Whether to include base R packages in the tree.
#'
#' @export

pkg_tree <- function(pkg, include_base = FALSE) {
  stopifnot(length(pkg) == 1)

  deps <- pkg_deps(pkg, include_base = include_base)

  draw_tree(names(deps)[1], deps, type = "")
}

draw_tree <- function(pkg, deps, type = "", prefix = "") {

  if (prefix != "") cat(prefix, "`--", sep = "")

  if (type != "") type <- paste0("[", tolower(type), "]")
  cat(pkg, type, "\n")

  for (dep_type in seq_along(deps[[pkg]])) {
    for (dep_pkg in seq_along(deps[[pkg]][[dep_type]])) {
      draw_tree(names(deps[[pkg]][[dep_type]])[dep_pkg], deps,
                type = substr(names(deps[[pkg]])[dep_type], 1, 1),
                prefix = paste0("  ", prefix))
    }
  }

  invisible()
}
