
## TODO: better default installation path
## TODO: implement multiple packages

#' Install packages
#'
#' @param pkgs The package(s) to install. Specific versions
#'   can be specified after a dash.
#' @param global Whether to install packages globally.
#' @param ask Whether to ask for confirmation before proceeding
#'   with the installation.
#' @param all Whether to install suggested and enhancaed packages
#'   as well, not only just the ones \code{pkgs} depend on, import from and
#'   link to. Set this to \code{TRUE} to make sure that you can run
#'   all examples and vignettes from the package(s).
#' @param download_dir Directory to store the downloaded
#'   packages in.
#' @return Invisibly a logical vector which is \code{TRUE} for
#'   successfully installed packages, and \code{FALSE} for others.
#'
#' @export

pkg_install <- function(pkgs, global = FALSE, ask = FALSE,
                        all = FALSE, download_dir = tempfile()) {

  pkgs <- as.character(pkgs)
  ask <- as.logical(ask)
  download_dir <- as.character(download_dir)

  stopifnot(all(!is.na(pkgs)))
  stopifnot(length(ask) == 1, !is.na(ask))
  stopifnot(length(download_dir) == 1, !is.na(download_dir))

  lib <- pkg_paths(global)
  
  ## Get dependencies
  deps <- pkg_deps(pkgs, all = all)

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
  types <- pkg_type_from_filename(files)
  sub_seqs <- same_bool_sub_seqs(types == "binary")
  for (i in seq_along(sub_seqs)) {
    filenames <- files[sub_seqs[[i]]$seq]
    type <- types[sub_seqs[[i]]$seq][1]
    install.packages(pkgs = filenames, repos = NULL,
                     lib = lib, dependencies = FALSE, quiet = TRUE,
                     type = type)
  }
}
