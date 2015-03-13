
base_packages <- c("base", "compiler", "datasets", "graphics",
                   "grDevices", "grid", "methods", "parallel",
                   "splines", "stats", "stats4", "tcltk", "tools",
                   "utils")

dep_types <- c("Imports", "Depends", "LinkingTo", "Enhances", "Suggests")

#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON

pkg_deps <- function(pkg, version = NULL) {

  version <- if (is.null(version)) "" else paste0("/", version)
  url <- paste0("http://crandb.r-pkg.org/-/pkgdeps/", pkg, version)

  deps <- fromJSON(content(GET(url), as = "text"), simplifyVector = FALSE)

  make_tree(pkg, deps)
}


make_tree <- function(tree, deps) {

  norm_dep <- function(x) {
    if (is.logical(x)) {
      list()
    } else {
      lapply(x, make_tree, deps)
    }
  }

  if (is.character(tree)) {
    list(package = tree, deps = norm_dep(deps[[tree]]))
  } else {
    tree
  }
}
