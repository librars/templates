#' LibRArs book.
#'
#' This format was adapted from the Springer manuscript package for Springer
#' monographs.
#'
#' @inheritParams bookdown::pdf_book
#' @param ... Arguments to \code{bookdown::pdf_book}
#' @return R Markdown output format to pass to \code{\link[bookdown::render_book]{render_book}}
#' @examples
#' \dontrun{
#' bookdown::render_book("MyArticle.Rmd")
#' }
#'
#' @export
book_tex <- function(..., keep_tex = TRUE, citation_package = 'none') {
  # locations of resource files in the package
  pkg_resource = function(...) {
    system.file(..., package = "librarstemplates")
  }

  tmpl = pkg_resource("rmarkdown", "templates", "book_tex", "resources", "template.tex")
  if (tmpl == "") {
    stop("Couldn't find pkg resource template")
  }

  bookdown::pdf_book(..., base_format = rmarkdown::pdf_document, template = tmpl, keep_tex = TRUE)
}
