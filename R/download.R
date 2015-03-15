
gh_cran_url <- "https://api.github.com/repos/cran/%s/tarball/%s"

#' Download packages from the CRAN mirror at Github
#'
#' The files will be named according to the package names and
#' version numbers.
#'
#' @param pkgs Package(s) to download. By default the latest
#'   versions are downloaded, a specific version number can
#'   be given after a dash.
#' @param dest_dir Destination directory.
#' @return A character vector containing the full paths of the
#'   downloaded packages.
#'
#' @export
#' @importFrom httr GET headers stop_for_status content
#' @examples
#' dest_dir <- tempdir()
#' pkg_download("testthat", dest_dir = dest_dir)
#' pkg_download("testthat-0.8.1", dest_dir = dest_dir)

pkg_download <- function(pkgs, dest_dir = ".") {
  pkgs <- as.character(pkgs)
  dest_dir <- as.character(dest_dir)

  stopifnot(all(!is.na(pkgs)))
  pkgtab <- split_pkg_names_versions(pkgs)

  stopifnot(all(!is.na(dest_dir)), length(dest_dir) == 1)
  stopifnot(dir_exists(dest_dir))

  res <- apply(pkgtab, 1, function(pkg) {
    url <- sprintf(gh_cran_url, pkg["name"], pkg["version"])
    message("Downloading ", pkg["name"], "-",
            if (pkg["version"] == "") "latest" else pkg["version"], " ...",
            appendLF = FALSE)
    file <- GET(url)
    stop_for_status(file)
    dest_file <- file.path(dest_dir, file_name_from_github_response(file))
    writeBin(content(file, as = "raw"), con = dest_file)
    message(" done.")
    dest_file
  })
  names(res) <- pkgs
  res
}

file_name_from_github_response <- function(resp) {
  cont <- headers(resp)$`content-disposition`
  cont <- sub("^.*filename=cran-", "", cont)
  sub("-0-g[0-9a-f]+", "", cont)
}
