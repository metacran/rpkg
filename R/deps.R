
base_packages <- c("R", "base", "compiler", "datasets", "graphics",
                   "grDevices", "grid", "methods", "parallel",
                   "splines", "stats", "stats4", "tcltk", "tools",
                   "utils")

dep_types <- c("Imports", "Depends", "LinkingTo", "Enhances", "Suggests")

pkg_deps <- function(pkgs, include_base = FALSE) {

  deps <- crandb_query(paste0("-/pkgdeps/", paste(pkgs, collapse = ",")))
  got_it <- pkgs %in% names(deps) | pkgs %in% sub("-.*$", "", names(deps))
  if (! all(got_it)) unknown_pkg_error(pkgs[!got_it])

  if (!include_base) {
    deps <- deps[ ! names(deps) %in% base_packages ]
    for (i in seq_along(deps)) {
      for (j in seq_along(deps[[i]])) {
        deps[[i]][[j]] <- drop_names(deps[[i]][[j]], base_packages)
      }
    }
  }

  deps
}

topo_sort <- function(deps) {
  res <- character(0)
  while (length(deps) > 0) {
    no_deps <- vapply(deps, function(x) sum(vapply(x, length, 1)), 1)
    zeroin <- names(deps)[no_deps == 0]
    if (!length(zeroin)) stop("Invalid dependency structure, loops?")
    res <- c(res, zeroin)
    for (i in seq_along(deps)) {
      for (j in seq_along(deps[[i]])) {
        deps[[i]][[j]] <- drop_names(deps[[i]][[j]], zeroin)
      }
    }
    deps <- deps[ ! names(deps) %in% zeroin ]
  }
  res
}
