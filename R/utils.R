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
  normalizePath(path, winslash = "/")
}

#' Converts all \r\n into \n
normalize_string_newlines <- function(input) {
  stringr::str_replace_all(input, "\\r\\n", "\n")
}

#' Gets the file content
get_file_content <- function(path) {
  readr::read_file(path)
}

#' Writes a file
write_file <- function(content, path) {
  writeLines(content, path)
}

#' Executes a batch of R commands in a different session
Rscript <- function(...) {
  xfun::Rscript(...)
}
