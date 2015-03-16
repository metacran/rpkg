
data_frame <- function(...) {

  args <- list(...)

  ## Replicate arguments if needed
  len <- vapply(args, length, numeric(1))
  stopifnot(length(setdiff(len, 1)) <= 1)
  len <- max(0, max(len))
  args <- lapply(args, function(x) rep(x, length.out = len))

  ## Names
  names <- as.character(names(args))
  length(names) <- length(args)
  names <- ifelse(
    is.na(names) | names == "",
    paste0("V", seq_along(args)),
    names)

  structure(args,
            class = "data.frame",
            names = names,
            row.names = seq_along(args[[1]]))
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

add_class <- function(x, class) {
  if (!inherits(x, class)) {
    class(x) <- c(class, class(x))
  }
  x
}

drop_non_existant <- function(x) {
  x[file.exists(x)]
}

try_extract <- function(x, elem) {
  res <- x[elem]
  if (is.na(res)) "" else res
}

dir_exists <- function(dir) {
  file.exists(dir) & file.info(dir)$isdir
}

split_pkg_names_versions <- function(pkgs) {

  if (!length(pkgs)) {
    return(data_frame(name = character(), version = character()))
  }

  pkgtab <- data_frame(
    name = sub("-.*$", "", pkgs),
    version = sub("^[^-]*-?", "", pkgs)
  )

  stopifnot(all(!is.na(pkgtab$name)))

  pkgtab
}

drop_null <- function(...) {
  l <- c(...)
  l[ !vapply(l, is.null, FALSE) ]
}

escape_package_name <- function(pkgs) {
  gsub(".", "\\.", pkgs, fixed = TRUE)
}

create_writable_dir <- function(dir) {
  if (file.exists(dir) && !is_dir(dir)) {
    stop("Directory exists and not a directory")
  }
  if (!file.exists(dir)) dir.create(dir)
}
