
## Package dependencies, we create a structure like this:

'
{ "package": "devtools",
  "depversion": "*",
  "version": "1.7.0",
  "deps":
    [{ "package": "httr",
       "depversion": ">= 0.4",
       "deptype": "Imports",
       "version": "0.6.1",
       "deps":
         [{ "package": "digest",
            "depversion": "*",
            "deptype": "Imports",
            "version": "0.6.8" },
          { "package": "jsonlite",
            "depversion": "*",
            "deptype": "Imports",
            "version": "0.9.14" },
          { "package": "mime",
            "depversion": "*",
            "deptype": "Imports",
            "version": "0.2" },
          ]},
     { "package": "RCurl",
       "depversion": "*",
       "deptype": "Imports",
       "version": "1.95-4.5" },
     { "package": "memoise",
       "depversion": "*",
       "deptype": "Imports",
       "version": "0.2.1" }]
}
'

dep_types<- c("Imports", "Depends", "LinkingTo", "Enhances", "Suggests")

pkg_deps <- function(pkg, recursive = c("Imports*", "Depends*",
                            "LinkingTo*")) {

  pkg <- as.character(pkg)
  stopifnot(length(pkg) == 1, !is.na(pkg))

  recursive <- as.character(recursive)

  stopifnot(all(recursive %in% c(dep_types, paste0(dep_types, "*"))))

  pkg_deps_internal(pkg, recursive)
}

#' @importFrom crandb package

pkg_deps_internal <- function(pkg, recursive, depversion = "*",
                              deptype = NA) {

  pkg_ver <- split_pkg_names_versions(pkg)
  version <- if (pkg_ver$version != "") pkg_ver$version

  deps <- list()

  desc <- try(silent = TRUE, package(pkg_ver$name, version = version))
  if (! inherits(desc, "try-error")) {
    recursive2 <- grep("\\*$", recursive, value = TRUE)
    recursive <- sub("\\*$", "", recursive)
    for (dt in recursive) deps <- c(deps, get_deps(desc[dt], recursive2))
  } else {
    desc <- list(Version = if (pkg_ver$version != "") pkg_ver$version else NA)
  }

  list(
    package = pkg_ver$name,
    depversion = depversion,
    deptype = deptype,
    version = desc$Version,
    deps = deps
  )
}

get_deps <- function(deplist, recursive) {
  deptype <- names(deplist)
  deplist <- deplist[[1]]

  ## Dependency on R itself
  deplist <- deplist[ names(deplist) != "R" ]

  ## No base packages
  deplist <- deplist[ ! is_base_package(names(deplist)) ]

  unname(mapply(
    SIMPLIFY = FALSE,
    names(deplist),
    unname(unlist(deplist)),
    FUN = function(n, dv) {
      pkg_deps_internal(n, recursive, depversion = dv,
                        deptype = deptype)

    }))
}
