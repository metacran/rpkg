
base_packages <- c("base", "compiler", "datasets", "graphics",
                   "grDevices", "grid", "methods", "parallel",
                   "splines", "stats", "stats4", "tcltk", "tools",
                   "utils")

dep_types <- c("Imports", "Depends", "LinkingTo", "Enhances", "Suggests")

pkg_deps <- function(pkgs, tree = TRUE, include_base = FALSE) {

  stopifnot(! tree || length(pkgs) == 1)

  deps <- crandb_query(paste0("-/pkgdeps/", paste(pkgs, collapse = ",")))
  got_it <- pkgs %in% names(deps) | pkgs %in% sub("-.*$", "", names(deps))
  if (! all(got_it)) unknown_pkg_error(pkgs[!got_it])

  if (!include_base) {
    deps <- deps[ ! names(deps) %in% base_packages ]
    for (i in seq_along(deps)) {
      deps[[i]][] <- lapply(deps, setdiff, y = base_packages)
    }
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


topo_sort <- function(deps) {
  res <- character(0)
  while (length(deps) > 0) {
    zeroin <- names(deps)[(vapply(deps, length, 1) == 0)]
    if (!length(zeroin)) stop("Invalid dependency structure, loops?")
    res <- c(res, zeroin)
    deps <- lapply(deps, setdiff, y = zeroin)
    deps <- deps[ ! names(deps) %in% zeroin ]
  }
  res
}
