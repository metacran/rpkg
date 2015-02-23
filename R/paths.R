
#' Default library directories
#'
#' If called without an argument, then it returns the library
#' directories. If called with an argument then it sets them.
#'
#' Note that the system-default R library directory (or directories
#' possibly) are always searched (and returned by \code{pkg_paths}.
#' 
#' @param new_value If specified, then the user-defined library
#'   directory list will be set to this value.
#' @return The current value of the default library directories.
#' 
#' @export
#' @examples
#' pkg_paths()

pkg_paths <- function(new_value) {
  .libPaths(new_value)
}
