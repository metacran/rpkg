
is_base_package <- function(pkgs) {

  pkgs <- as.character(pkgs)

  pkg_ver <- split_pkg_names_versions(pkgs)

  pkg_ver$name %in% c("base", "compiler", "datasets", "graphics",
                      "grDevices", "grid", "methods", "parallel",
                      "splines", "stats", "stats4", "tcltk", "tools",
                      "utils")
}
