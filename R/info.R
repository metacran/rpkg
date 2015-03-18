
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

#' Open URL(s) of packages in a browser
#'
#' Opens the URLs of the given packages in the web browser.
#' If package versions are also given, then the URLs are
#' taken from the specified versions of the packages.
#'
#' If a package has multiple URLs, then all are opnened.
#'
#' @section Note:
#' On OSX, there seems to be bug that prevents opening more than
#' three tabs within a short period of time, so some URLs might not open.
#'
#' @param pkgs The package(s), optionally with version numbers after a
#'   dash.
#' @param browser Passed to \code{utils::browseURL}.
#' @return The URLs of the packages, in a list, invisibly.
#'
#' @export
#' @importFrom utils browseURL

pkg_browse <- function(pkgs, browser = getOption("browser")) {
  urls <- crandb_get_url(pkgs)
  if (!identical(browser, FALSE)) {
    sapply(unlist(urls), browseURL, browser = browser)
  }
  invisible(urls)
}
