
base_packages <- c("base", "compiler", "datasets", "graphics",
                   "grDevices", "grid", "methods", "parallel",
                   "splines", "stats", "stats4", "tcltk", "tools",
                   "utils")

dep_types <- c("Imports", "Depends", "LinkingTo", "Enhances", "Suggests")

#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON

pkg_deps <- function(pkg, tree = TRUE, include_base = FALSE) {

  pkgtab <- split_pkg_names_versions(pkg)
  if (pkgtab$version != "") pkgtab$version <- paste0("/", pkgtab$version)

  url <- paste0("http://crandb.r-pkg.org/-/pkgdeps/",
                pkgtab$name, pkgtab$version)

  deps <- fromJSON(content(GET(url), as = "text"), simplifyVector = FALSE)

  if (!include_base) {
    deps <- deps[ ! names(deps) %in% base_packages ]
    deps[] <- lapply(deps, setdiff, y = base_packages)
  }

  if (tree) {
    make_tree(names(deps)[1], deps)
  } else {
    deps
  }
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
