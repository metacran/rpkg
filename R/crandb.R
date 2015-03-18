
crandb_url <- "http://crandb.r-pkg.org/"

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
  crandb_query("-/versions", keys = pkgs)
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
      stop("Unknown CRAN package(s): ",
           paste(unique(setdiff(pkgs[no_ver], names(vers))), collapse = ", "))
    }
    pkgs[no_ver] <- paste0(pkgs[no_ver], '-', sapply(vers, "[[", "version"))
  }
  pkgs
}
