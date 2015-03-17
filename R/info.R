
#' Information about a package
#'
#' Print the description of a CRAN package. It does not have
#' to be installed.
#'
#' @param pkg Package name and optionally version after a dash.
#' @param lib Search paths to see if the package is installed.
#' @return An rpkg package, invisibly.
#'
#' @export

pkg_info <- function(pkg, lib = pkg_paths()) {

  pkg <- as.character(pkg)
  lib <- as.character(lib)

  stopifnot(length(pkg) == 1, !is.na(pkg))
  stopifnot(all(!is.na(lib)))

  regi <- crandb_pkgs(pkg)

  regi$InstalledVersion <- get_installed_version(pkgtab$name, lib = lib)

  inst_text <- if (is.null(regi$InstalledVersion)) {
    "not installed"
  } else if (regi$Version == regi$InstalledVersion) {
    "installed"
  } else {
    paste("version", regi$InstalledVersion, "installed")
  }

  pkgtab <- split_pkg_names_versions(pkg)

  cat(sep = "", pkgtab$name, "-", regi$Version, " -- ", inst_text, "\n",
      "---------------------------\n")

  print(regi)
}
