
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
