#' LibRArs book
#'
#' This format was adapted from the Springer manuscript package for Springer
#' monographs.
#'
#' @inheritParams bookdown::html_chapters
#' @param ... Arguments to \code{bookdown::html_chapters}.
#' @return R Markdown output format to pass to
#'   \code{\link[bookdown::render_book]{render_book}}.
#' @examples
#' \dontrun{
#' bookdown::render_book("MyArticle.Rmd")
#' }
#'
#' @export
book_html_old <- function(..., number_sections = TRUE) {
  tmpl = get_pkg_resource("rmarkdown", "templates",
                          "book_html", "resources", "template.html")
  if (tmpl == "") {
    stop("Couldn't find pkg resource template")
  }

  bookdown::html_chapters(...,
                          base_format = rmarkdown::html_document,
                          template = tmpl,
                          number_sections = number_sections)
}

#' LibRArs book
#'
#' This format was adapted from the Springer manuscript package for Springer
#' monographs.
#'
#' @export
book_html_format <- function() {

}
