#' LibRArs main authoring function.
#'
#' Entry function to convert an Rmd LibRArs book into the desired format as
#' specified in the \code{output} parameter of the yaml front matter.
#'
#' @param input The file name representing the book to convert into
#'   the desired format.
#' @return The book in the final desired format.
#' @examples
#' \dontrun{
#' librars::make("MyArticle.Rmd")
#' }
#'
#' @export
make <- function(input) {
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

#' Deploys resources
#'
#' @export
deploy <- function() {

}
