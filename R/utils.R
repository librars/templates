#' Gets resources from this package.
#'
#' Used internally to provide resources inside this package.
#'
#' @inheritParams base::system.file
get_pkg_resource <- function(...) {
  system.file(..., package = "librarstemplates")
}
