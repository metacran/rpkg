
gh_cran_url <- "https://api.github.com/repos/cran/%s/tarball/%s"
cran_mirror <- "http://cran.rstudio.com"

r_minor_version <- function() {
  ver <- R.Version()
  paste0(ver$major, ".", strsplit(ver$minor, ".", fixed = TRUE)[[1]][1])
}

get_pkg_type <- function() {
  "both"
}

cran_file <- function(package, version, type = get_pkg_type(),
                      r_minor = r_minor_version()) {

  if (type == "both") {
    c(cran_file(package, version, type = "binary", r_minor = r_minor),
      cran_file(package, version, type = "source", r_minor = r_minor))
  } else if (type == "binary") {
    cran_file(package, version, type = .Platform$pkgType, r_minor = r_minor)
  } else if (type == "source") {
    c(sprintf("%s/src/contrib/%s_%s.tar.gz", cran_mirror, package, version),
      sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz", cran_mirror,
              package, package, version))
  } else if (type == "win.binary") {
    sprintf("%s/bin/windows/contrib/%s/%s_%s.zip", cran_mirror, r_minor,
            package, version)
  } else if (type == "mac.binary.mavericks") {
    sprintf("%s/bin/macosx/mavericks/contrib/%s/%s_%s.tgz", cran_mirror,
            r_minor, package, version)
  } else if (type == "mac.binary") {
    sprintf("%s/bin/macosx/contrib/%s/%s_%s.tgz", cran_mirror, r_minor,
            package, version)
  } else {
    stop("Unknown package type: ", type, " see ?options.")
  }
}

github_file <- function(package, version, type = get_pkg_type(),
                        r_minor = r_minor_version()) {
  sprintf(gh_cran_url, package, version)
}

download_urls <- function(pkgs) {
  pkgtab <- split_pkg_names_versions(pkgs)
  stopifnot(all(pkgtab$version != ""))

  lapply(seq_along(pkgs), function(i) {
    pkg <- pkgtab[i,]
    c(cran_file(pkg["name"], pkg["version"]),
      github_file(pkg["name"], pkg["version"]))
  })
}

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
#' @importFrom httr GET status_code stop_for_status write_disk
#' @examples
#' dest_dir <- tempdir()
#' pkg_download("testthat", dest_dir = dest_dir)
#' pkg_download("testthat-0.8.1", dest_dir = dest_dir)

pkg_download <- function(pkgs, dest_dir = ".") {
  pkgs <- as.character(pkgs)
  dest_dir <- as.character(dest_dir)

  stopifnot(all(!is.na(pkgs)))

  stopifnot(all(!is.na(dest_dir)), length(dest_dir) == 1)
  stopifnot(dir_exists(dest_dir))

  urls <- download_urls(get_latest_versions(pkgs))
  res <- vapply(seq_along(pkgs), FUN.VALUE = "", FUN = function(i) {
    url <- urls[[i]]
    message("Downloading ", appendLF = FALSE)
    for (u in url) {
      dest_file <- file.path(dest_dir, filename_from_url(u, pkgs[i]))
      message(basename(u), "... ", appendLF = FALSE)
      if (res <- try_download(u, dest_file)) break
    }
    message(if (res) " done." else "ERROR.")
    if (!res) stop("Cannot download package ", pkgs[i])
    dest_file
  })

  names(res) <- pkgs
  invisible(res)
}

filename_from_url <- function(url, pkg) {
  if (grepl("^https://[^/\\.]*\\.github.com/", url)) {
    paste0(sub("-", "_", pkg), ".tar.gz")
  } else {
    basename(url)
  }
}

try_download <- function(url, dest_file) {
  if (file.exists(dest_file)) return(TRUE)
  resp <- GET(url, write_disk(dest_file))
  if (status_code(resp) != 200) {
    unlink(dest_file)
    FALSE
  } else {
    TRUE
  }
}
