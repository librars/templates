dir_exists <- function(x) {
  utils::file_test('-d', x)
}