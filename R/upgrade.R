
## Do we need to upgrade these packages, or we
## already have these versions (or newer) installed
## new_versions should include available versions
## as well. The results is
## - install: package is not installed
## - upgrade: package is installed but upgrade is needed
## - no:      package is installed and up to date
needs_upgrade <- function(new_versions) {
  pkg_tab <- split_pkg_names_versions(new_versions)
  stopifnot(all(pkg_tab$version != ""))

  res <- apply(pkg_tab, 1, function(pkg) {
    iver <- get_installed_version(pkg["name"])
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
#' @param lib Library paths to look at.
#' @return A data frame with columns: package, installed, latest.

#' @export

pkg_outdated <- function(filter = "", lib = pkg_paths()) {
  pkgs <- pkg_list(filter = filter, lib = lib)
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

  uptab <- uptab[order(uptab$package), ]

  cat(sep = "", length(pkgs), " packages, ",
      sum(status == "non-cran"), " not from CRAN, ",
      sum(status == "uptodate"), " up to date, ",
      sum(status == "obsolete"), " needs upgrade")

  if (nrow(uptab)) {
    cat(":\n")
    print(uptab)
  } else {
    cat(".\n")
  }

  invisible(uptab)
}
