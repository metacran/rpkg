
#' Dependency tree of a package
#'
#' @param pkg Package name, optionally with version number after a dash.
#' @param recursive What kind of dependencies to collect. If a type ends
#'   with a star, that type of dependency is collected recursively.
#'
#' @export

pkg_tree <- function(pkg, recursive = c("Imports*", "Depends*",
                            "LinkingTo*")) {

  draw_tree(pkg_deps(pkg, recursive = recursive))
}

draw_tree <- function(deps, prefix = 0) {

  if (prefix != 0) cat(rep(" ", prefix - 2), "`-", sep = "")
  cat(deps$package, "\n")
  for (el in deps$deps) {
    if (length(el)) draw_tree(el, prefix = prefix + 2)
  }

  invisible()
}
