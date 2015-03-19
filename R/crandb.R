
crandb_url <- "http://crandb.r-pkg.org/"
cran_pkg_url <- "http://cran.r-project.org/package=%s"

crandb_param <- function(name, value)
  UseMethod("crandb_param")

#' @export

crandb_param.character <- function(name, value) {
  paste0(
    name,
    "=[",
    paste0('"', value, '"', collapse=","),
    "]"
  )
}

#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON

crandb_query <- function(view, ...) {
  raw_params <- list(...)
  params <- paste0(
    "?",
    paste(mapply(names(raw_params), raw_params, FUN = crandb_param),
          collapse = "&")
  )

  url <- paste0(crandb_url, view, params)

  fromJSON(content(GET(url), as = "text"), simplifyVector = FALSE)
}

crandb_pkgs <- function(pkgs) {
  res <- crandb_query("-/versions", keys = pkgs)
  for (i in seq_along(res)) class(res[[i]]) <- "rpkg"
  class(res) <- "rpkgs"
  res
}

crandb_latest_versions <- function(pkgs) {
  qres <- crandb_query("-/desc", keys = pkgs)
  structure(vapply(qres, "[[", "", "version"), names = names(qres))
}

get_latest_versions <- function(pkgs) {
  pkgtab <- split_pkg_names_versions(pkgs)
  no_ver <- which(pkgtab$version == "")
  if (length(no_ver)) {
    vers <- crandb_query("-/desc", keys = pkgs[no_ver])
    if (length(vers) != length(no_ver)) {
      unknown_pkg_error(setdiff(pkgs[no_ver], names(vers)))
    }
    pkgs[no_ver] <- paste0(pkgs[no_ver], '-', sapply(vers, "[[", "version"))
  }
  pkgs
}

cran_pkg_urls <- function(pkgs) {
  pkgs <- split_pkg_names_versions(pkgs)$name
  sprintf(cran_pkg_url, pkgs)
}

unknown_pkg_error <- function(pkgs) {
  stop("Unknown CRAN package(s): ",
       paste(unique(pkgs), collapse = ", "), call. = FALSE)
}

crandb_get_url <- function(pkgs) {
  urls <- lapply(crandb_pkgs(pkgs), "[[", "URL")

  if (length(urls) < length(pkgs)) {
    unknown_pkg_error(setdiff(pkgs, names(urls)))
  }

  urls <- mapply(urls, cran_pkg_urls(pkgs), FUN = function(u1, u2) {
    if (!is.null(u1)) u1 else u2
  })
  lapply(urls, function(x) trim(strsplit(x, ",")[[1]]))
}
