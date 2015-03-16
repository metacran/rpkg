
#' Package manager for CRAN and other repositories
#'
#' @name rpkg
#' @docType package
NULL

## API:
##
## * pkg_install    - install
##   pkg_remove     - remove
## * pkg_download   - download (but do not install) package
##   pkg_upgrade    - upgrade
##   pkg_reinstall  - reinstall same version
##   pkg_clean_deps - remove packages that were installed
##                    only as dependencies and are not needed any more
## * pkg_list       - list installed packages
##   pkg_search     - search for packages
##   pkg_info       - display information about package
##   pkg_outdated   - show installed packages that are outdated
##   pkg_doctor     - sanity checks
## * pkg_paths      - set/get package directories
## * pkg_tree       - package(s) and their dependency tree
##   pkg_bug        - report bug for a package
##   pkg_browse     - home page of a package (or rpkg itself), or repo URL
##
## Configuration (later):
##
## pkg_config    - get/set configuration options
## 
## Account management (later):
##
## pkg_login     - log in to the rpkg service
## pkg_logout    - log out
## pkg_whoami    - print logged in user
## pkg_adduser   - add a new user
##
## Logging and statistics (later):
##
## pkg_log       - entries from the log, about a package or everything
## pkg_downloads - download statistics about a package or general
##
## Package creation, binaries and distribution (later):
##
## pkg_create       - create an empty package
## pkg_submit       - submit it to rpkg
## pkg_bump_version - increase version number
##
## pkg_make_osx     - make a binary OSX package
## pkg_make_win     - make a binary Windows package
## pkg_make_debian  - etc.
## pkg_make_ubuntu
## pkg_make_centos
## ...
##
## Repo management (later):
##
## later...
