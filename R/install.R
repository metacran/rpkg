
## TODO: better default installation path
## TODO: implement multiple packages
pkg_install <- function(pkgs, lib = pkg_paths()[1],
                        ask = interactive(), download_dir = tempfile()) {

  pkgs <- as.character(pkgs)
  lib <- as.character(lib)
  ask <- as.logical(ask)
  download_dir <- as.character(download_dir)

  stopifnot(length(pkgs) == 1, !is.na(pkgs))
  stopifnot(length(lib) == 1, !is.na(lib))
  stopifnot(length(ask) == 1, !is.na(ask))
  stopifnot(length(download_dir) == 1, !is.na(download_dir))
  
  ## Get dependencies
  deps <- pkg_deps(pkgs, tree = FALSE)

  ## Create installation order
  order <- topo_sort(deps)

  ## Check what we have
  to_install <- needs_upgrade(order)
  
  ## Is there anything to install?
  if (any(to_install != "no")) {
    if (ask) {
      msg <- paste0("Installing ", tail(order, 1), ",  altogether ",
                    sum(to_install == "install"), " new packages, ",
                    "upgrading ", sum(to_install == "upgrade"), ". ",
                    "Proceed?")
      message(strwrap(msg))
      if (menu(c("Yes", "No")) != 1) {
        message("Quiting.")
        return(invisible(structure(rep(FALSE, length(pkgs)), names = pkgs)))
      }
    }
  } else {
    if (ask) {
      message("Package ", tail(order,1),
              " is up to date, nothing to install.")
    }
    return(invisible(structure(rep(TRUE, length(pkgs)), names = pkgs)))
  }

  ## Download all
  create_writable_dir(download_dir)
  files <- pkg_download(order[to_install != "no"], dest_dir = download_dir)
  
  ## Install all
  for (f in files) {
    if (ask) message("Installing ", basename(f), " ...", appendLF = FALSE)
    install.packages(pkgs = f, repos = NULL, lib = lib,
                     dependencies = FALSE, quiet = TRUE, type = "source")
    if (ask) message(" done.")
  }
    
  invisible(structure(rep(TRUE, length(pkgs)), names = pkgs))
}
