#' LibRArs book
#'
#' This format was adapted from the Springer manuscript package for Springer
#' monographs.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}.
#' @return R Markdown output format to pass to
#'   \code{\link[bookdown::render]{render}}.
#' @examples
#' \dontrun{
#' rmarkdown::render("MyArticle.Rmd", book_tex_format())
#' }
#'
#' @export
book_tex_format <- function(...) {
  format <- rmarkdown::pdf_document

  # Augment by adding a pre-knit step
  format$pre_knit <- function(input, ...) {
    pre_process(input, ...)
  }

  format
}
