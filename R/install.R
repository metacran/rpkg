
## TODO: better default installation path
## TODO: implement multiple packages

#' Install packages
#'
#' @param pkgs The package(s) to install. Specific versions
#'   can be specified after a dash.
#' @param lib Library directory to install the packages to.
#' @param ask Whether to ask for confirmation before proceeding
#'   with the installation.
#' @param download_dir Directory to store the downloaded
#'   packages in.
#' @return Invisibly a logical vector which is \code{TRUE} for
#'   successfully installed packages, and \code{FALSE} for others.
#'
#' @export
#' @examples
#' pkg_install("httr", lib = "/tmp")
#' pkg_install("magrittr-1.0.0", lib = "/tmp", ask = FALSE)

pkg_install <- function(pkgs, lib = pkg_paths()[1],
                        ask = interactive(), download_dir = tempfile()) {

  pkgs <- as.character(pkgs)
  lib <- as.character(lib)
  ask <- as.logical(ask)
  download_dir <- as.character(download_dir)

  stopifnot(all(!is.na(pkgs)))
  stopifnot(length(lib) == 1, !is.na(lib))
  stopifnot(length(ask) == 1, !is.na(ask))
  stopifnot(length(download_dir) == 1, !is.na(download_dir))
  
  ## Get dependencies
  deps <- pkg_deps(pkgs, tree = FALSE)

  ## Create installation order
  order <- topo_sort(deps)

  ## Check what we have
  to_install <- needs_upgrade(order, lib = lib)

  ## Is there anything to install?
  if (any(to_install != "no")) {
    msg <- paste0(
      "Installing ", paste(pkgs, collapse = ", "), ",  altogether ",
      sum(to_install == "install"), " new packages, upgrading ",
      sum(to_install == "upgrade"), "."
    )
    message(paste(strwrap(msg, exdent = 1), collapse = "\n"))
    if (ask) {
      if (menu(c("Yes", "No")) != 1) {
        message("Quiting.")
        return(invisible(structure(rep(FALSE, length(pkgs)), names = pkgs)))
      }
    }
  } else {
    message("Packages are up to date, nothing to install.")
    return(invisible(structure(rep(TRUE, length(pkgs)), names = pkgs)))
  }

  ## Download all
  create_writable_dir(download_dir)
  files <- pkg_download(order[to_install != "no"], dest_dir = download_dir)
  
  ## Install all
  msg <- paste0("Installing ", paste(basename(files), collapse = ", "), ".")
  msg <- paste(strwrap(msg), collapse = "\n ")

  message(msg)
  install_files(files, lib = lib)
  message("Done.")

  invisible(structure(rep(TRUE, length(pkgs)), names = pkgs))
}

install_files <- function(files, lib) {
  create_writable_dir(lib)
  install.packages(pkgs = files, repos = NULL, lib = lib,
                   dependencies = FALSE, quiet = TRUE)
}
