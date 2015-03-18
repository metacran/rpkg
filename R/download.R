
gh_cran_url <- "https://api.github.com/repos/cran/%s/tarball/%s"
cran_mirror <- "http://cran.rstudio.com"

r_minor_version <- function() {
  ver <- R.Version()
  paste0(ver$major, ".", strsplit(ver$minor, ".", fixed = TRUE)[[1]][1])
}

get_pkg_type <- function() {
  getOption("pkgType", .Platform$pkgType)
}

cran_file <- function(package, version, type = get_pkg_type(),
                      r_minor = r_minor_version()) {
  if (type == "source") {
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
  type <- get_pkg_type()

  lapply(seq_along(pkgs), function(i) {
    pkg <- pkgtab[i,]
    if (type == "source") {
      c(cran_file(pkg["name"], pkg["version"], type = "source"),
        github_file(pkg["name"], pkg["version"], type = "soruce"))
    } else {
      c(cran_file(pkg["name"], pkg["version"], type = type),
        cran_file(pkg["name"], pkg["version"], type = "source"),
        github_file(pkg["name"], pkg["version"], type = type))
    }
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
#' @importFrom httr GET status_code stop_for_status content
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
  res <- vapply(urls, FUN.VALUE = "", FUN = function(url) {
    message("Downloading ", appendLF = FALSE)
    for (u in url) {
      message(basename(u), "... ", appendLF = FALSE)
      if (status_code(resp <- GET(u)) == 200) break
    }
    stop_for_status(resp)
    dest_file <- file.path(dest_dir, filename_from_response(resp))
    writeBin(content(resp, as = "raw"), con = dest_file)
    message(" done.")
    dest_file
  })

  names(res) <- pkgs
  invisible(res)
}

#' @importFrom httr headers

filename_from_response <- function(resp) {
  if (grepl("^https://[^/\\.]*\\.github.com/", resp$url)) {
    cont <- headers(resp)$`content-disposition`
    cont <- sub("^.*filename=cran-", "", cont)
    sub("-0-g[0-9a-f]+", "", cont)
  } else {
    basename(resp$url)
  }
}
