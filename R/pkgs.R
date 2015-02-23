
#' @export
#' @method print rpkgs
#' @importFrom prettyunits time_ago

print.rpkgs <- function(x, ...) {
  df <- data_frame(
    package = paste(
      sep = "-",
      vapply(x, try_extract, "", "Package"),
      vapply(x, try_extract, "", "Version")
    ),
    repo = vapply(x, try_extract, "", "Repository"),
    date = vapply(x, try_extract, "", "Date/Publication")
  )

  df$date[df$date != ""] <-
    time_ago(as.Date(df$date[df$date != ""]), format = "terse")

  print(df)
  invisible(x)
}

#' @export
#' @method print rpkg

print.rpkg <- function(x, ...) {
  ## TODO
  print(unclass(x), ...)
}

read_package_rds <- function(rds_files) {
  rds_files <- as.character(rds_files)

  pkgs <- lapply(lapply(rds_files, readRDS), rds_to_rpkg)

  class(pkgs) <- "rpkgs"

  pkgs
}

rds_to_rpkg <- function(rds) {
  structure(rds$DESCRIPTION, class = "rpkg")
}
