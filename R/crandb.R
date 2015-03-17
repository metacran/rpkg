
crandb_url <- "http://crandb.r-pkg.org/"

crandb_param <- function(name, value)
  UseMethod("crandb_param")

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
