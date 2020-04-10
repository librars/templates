#' LibRArs book
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
