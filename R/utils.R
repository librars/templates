#' Gets resources from this package
#'
#' Used internally to provide resources inside this package.
#'
#' @inheritParams base::system.file
get_pkg_resource <- function(...) {
  system.file(..., package = "librarstemplates")
}

#' Normalizes a path to the platform
normalize_path <- function(path) {
  normalizePath(path)
}

#' Converts all \r\n into \n
normalize_string_newlines <- function(input) {
  stringr::str_replace_all(input, "\\r\\n", "\n")
}
