#' LibRArs main rafting function.
#'
#' Wrapper for same function in rmarkdown.
#'
#' @param file The name of the Rmd file to be created from the draft.
#' @param template The template to use in LibRArs.
#' @param ... The params to pass to rmarkdown::draft
#' @return The book in the final desired format.
#' @examples
#' \dontrun{
#' librarstemplates::draft("MyArticle.Rmd", "book_tex")
#' }
#'
#' @export
draft <- function(file, template, package = "librarstemplates", ...) {
  rmarkdown::draft(file, template, package = package, ...)
}
