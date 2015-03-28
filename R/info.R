
#' Information about a package
#'
#' Print the description of a CRAN package. It does not have
#' to be installed.
#'
#' @param pkg Package name and optionally version after a dash.
#' @param global Whether to check for installed packages globally
#'   or locally.
#' @return An rpkg package, invisibly.
#'
#' @export

pkg_info <- function(pkg, global = FALSE) {

  pkg <- as.character(pkg)

  lib <- pkg_paths(global)

  stopifnot(length(pkg) == 1, !is.na(pkg))
  stopifnot(all(!is.na(lib)))

  regi <- crandb_pkgs(pkg)[[1]]

  pkgtab <- split_pkg_names_versions(pkg)

  regi$InstalledVersion <- get_installed_version(pkgtab$name, lib = lib)

  inst_text <- if (is.null(regi$InstalledVersion)) {
    "not installed"
  } else if (regi$Version == regi$InstalledVersion) {
    "installed"
  } else {
    paste("version", regi$InstalledVersion, "installed")
  }

  regi
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

#' Report a bug for a package
#'
#' Open the web page of the package's bug tracker, or an email
#' client to email the package's maintainer.
#'
#' This command is similar to \code{utils::bug.report}.
#'
#' @param pkg Name of the package.
#'
#' @export

pkg_bug <- function(pkg) {
  pkg <- as.character(pkg)
  stopifnot(!is.na(pkg), length(pkg) == 1)

  cdb_pkg <- crandb_pkgs(pkg)
  if (!length(cdb_pkg)) unknown_pkg_error(pkg)

  message("Maintainer: ", cdb_pkg[[1]]$Maintainer)
  if (!is.null(cdb_pkg[[1]]$BugReports)) {
    message("Opening bug report URL: ", cdb_pkg[[1]]$BugReports)
    browseURL(cdb_pkg[[1]]$BugReports)
  } else {
    message("Opening email client...")
    email <- sub("[^<]*<([^>]+)>.*", "\\1", cdb_pkg[[1]]$Maintainer)
    browseURL(paste0("mailto:", email))
  }
  message("Don't forget to include the output of sessionInfo()\n",
          "and a reproducible example")

  invisible()
}
