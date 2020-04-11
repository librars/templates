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

#' Reads properly UTF8-encoded files
read_utf8 <- function(file) {
  if (inherits(file, 'connection')) {
    con <- file
  } else {
    con <- base::file(file, encoding = 'UTF-8')
    on.exit(close(con), add = TRUE)
  }

  enc2utf8(readLines(con, warn = FALSE))
}

#' Writes properly encoded UTF8 values
write_utf8 <- function(text, con, ...) {
  opts <- options(encoding = "native.enc"); # Save original options
  on.exit(options(opts), add = TRUE)

  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}

#' Invokes \code{yaml::load}
yaml_load <- function(...) yaml::yaml.load(..., eval.expr = TRUE)

#' Loads a YAML file.
yaml_load_file <- function(input, ...) yaml_load(read_utf8(input), ...)

#' Find the function in this package and executes it
#' 
#' @param fun_name The function name to look for.
#' @param ... The arguments to pass to the function invocation.
#' @return The invocation result.
execute_pkg_function <- function(fun_name, ...) {
  fun <- get_pkg_function(fun_name)
  if (is.null(fun)) {
    stop("Cannot find function '", fun_name, "'")
  }

  do.call(fun, list(...))
}

#' Find the function in this package
#' 
#' @param fun_name The function name to look for.
#' @return The function object if found.
get_pkg_function <- function(fun_name) {
  res <- NULL

  if (is.character(fun_name) && nchar(fun_name) > 0) {
    tryCatch({
      res <- get(fun_name, asNamespace("librarstemplates"))
    }, error = function() {
      res <- NULL
    }, warning = function() {
      res <- NULL
    })
  }

  res
}
