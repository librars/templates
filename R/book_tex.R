#' Format function for book_tex template
#'
#' This format renders a PDF and selects the template available
#' in the folder at the moment of rendering.
#'
#' @inheritParams bookdown::pdf_book
#' @param ... Arguments to \code{bookdown::pdf_book}.
#' @return R Markdown output format to pass to
#'   \code{\link[bookdown::render_book]{render_book}}.
#' @examples
#' \dontrun{
#' bookdown::render_book("MyArticle.Rmd", librarstemplates::book_tex_format())
#' }
#'
#' @export
book_tex_format <- function(...) {
  # Function \code{prepare_template} takes care of having template.tex
  # in place in the same directory where the files to compile are
  bookdown::pdf_book(..., template = "template.tex")
}

#' Pre processing function for book_tex template
#' 
#' In this context, we need to apply the fix/hack as explained
#' in \link{https://github.com/rstudio/bookdown/issues/750}.
#' 
#' @param dir The directory where the files are located.
#' @export
book_tex_pre <- function(dir) {
  package <- "librarstemplates"
  template_name <- "book_tex"

  template_path <- template.get_path(template_name, package = package) # Template folder
  if (!nzchar(template_path) || !dir_exists(template_path)) {
    stop("The template '", template_name, "' was not found in the ", package, " package")
  }

  remthm_rmd_path <- file.path(template_path, "resources", "rem_thm_defs.Rmd")
  if (!nzchar(remthm_rmd_path) || !file_exists(remthm_rmd_path)) {
    stop(paste("Preprocessor could not find file", remthm_rmd_path))
  }
  remthm_rmd <- get_file_content(remthm_rmd_path)

  index_rmd_path <- file.path(dir, INDEX_RMD_FILENAME)
  if (!file_exists(index_rmd_path)) {
    stop(paste("Preprocessor could not find file", index_rmd_path))
  }
  index_rmd <- get_file_content(index_rmd_path)

  new_index_rmd <- paste(index_rmd, remthm_rmd, sep = "\n\n")
  write_file(new_index_rmd, index_rmd_path)
}

#' Post processing function for book_tex template
#' 
#' @param dir The directory where the compilation files are located.
#' @param original_dir The directory from which source files originated.
#' @export
book_tex_post <- function(dir, original_dir) {
  base_path <- file.path(dir, OUT_DIRNAME)

  to_copy <- file.path(base_path, list.files(path = base_path, pattern = "\\.pdf$"))
  file.copy(from = to_copy, to = original_dir)

  # A bug in bookdown might cause out dir not to be generated
  to_copy2 <- file.path(dir, list.files(path = dir, pattern = "\\.pdf$"))
  file.copy(from = to_copy2, to = original_dir)
}
