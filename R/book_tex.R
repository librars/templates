#' Format function for book_tex template
#'
#' This format renders a PDF and selects the template available
#' in the folder at the moment of rendering.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}.
#' @return R Markdown output format to pass to
#'   \code{\link[bookdown::render_book]{render_book}}.
#' @examples
#' \dontrun{
#' bookdown::render_book("MyArticle.Rmd", librarstemplates::book_tex_format())
#' }
#'
#' @export
book_tex_format <- function(...) {
  rmarkdown::pdf_document(..., template = "template.tex")
}

#' Pre processing function for book_tex template
#' 
#' @param dir The directory where the files are located.
#' @export
book_tex_pre <- function(dir) {

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
