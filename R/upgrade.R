
## Do we need to upgrade these packages, or we
## already have these versions (or newer) installed
## new_versions should include available versions
## as well. The results is
## - install: package is not installed
## - upgrade: package is installed but upgrade is needed
## - no:      package is installed and up to date
needs_upgrade <- function(new_versions, lib) {
  pkg_tab <- split_pkg_names_versions(new_versions)
  stopifnot(all(pkg_tab$version != ""))

  res <- apply(pkg_tab, 1, function(pkg) {
    iver <- get_installed_version(pkg["name"], lib = lib)
    if (is.null(iver)) {
      "install"
    } else if (compareVersion(iver, pkg["version"]) == -1) {
      "upgrade"
    } else {
      "no"
    }
  })
  
  structure(res, names = new_versions)
}


#' List outdated packages
#'
#' @param filter Regular expression to filter packages to check.
#' @param global Whether to consider global or local packages.
#' @param print Whether to print list of outdated packages.
#' @return A data frame with columns: package, installed, latest.

#' @export

pkg_outdated <- function(filter = "", global = FALSE, print = TRUE) {

  lib <- pkg_paths(global)

  pkgs <- pkg_list(filter = filter, global = global)
  names <- vapply(pkgs, "[[", "", "Package")

  latest <- crandb_latest_versions(names)

  status <- vapply(pkgs, FUN.VALUE = "", function(pkg) {
    if (! pkg[["Package"]] %in% names(latest)) {
      "non-cran"
    } else if (compareVersion(pkg[["Version"]],
                              latest[pkg[["Package"]]]) != -1) {
      "uptodate"
    } else {
      "obsolete"
    }
  })

  uptab <- data_frame(
    package = names[status == "obsolete"],
    installed = vapply(pkgs, "[[", "", "Version")[status == "obsolete"],
    latest = latest[ names[status == "obsolete"] ]
  )

  attr(uptab, "uptodate") <- sum(status == "uptodate")
  attr(uptab, "non-cran") <- sum(status == "non-cran")

  uptab <- uptab[order(uptab$package), ]

  if (print) {
    outdated_summary(uptab)
  } else {
    uptab
  }
}

outdated_summary <- function(uptab) {
  total <- attr(uptab, "non-cran") + attr(uptab, "uptodate") + nrow(uptab)
  cat(sep = "", total, " packages, ",
      attr(uptab, "non-cran"), " not from CRAN, ",
      attr(uptab, "uptodate"), " up to date, ",
      nrow(uptab), " needs upgrade")

  if (nrow(uptab)) {
    cat(":\n")
    print(uptab)
  } else {
    cat(".\n")
  }

  invisible()
}

#' Upgrade packages
#'
#' @param filter Regular expression to filter the packages to update.
#' @param global Whether to upgrade global or local packages.
#' @param ... Extra arguments are passed to \code{\link{pkg_install}}.
#' @return Invisibly a logical vector which is \code{TRUE} for
#'   successfully installed packages, and \code{FALSE} for others.
#' @export

pkg_upgrade <- function(filter = "", global = FALSE, ...) {
  out <- pkg_outdated(filter = filter, global = global, print = FALSE)
  if (nrow(out)) {
    pkg_install(out$package, global = global, ...)
  } else {
    structure(character(), names = character())
  }
}
