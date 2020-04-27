#' Format function for book_tex template
#'
#' This format renders a PDF and selects the template available
#' in the folder at the moment of rendering.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}.
#' @param clean_after A value indicating whether to clean after or not.
#' @return R Markdown output format to pass to
#'   \code{\link[librarstemplates::render]{render}}.
#' @examples
#' \dontrun{
#' librarstemplates::render("./book/", librarstemplates::book_tex_format())
#' }
#'
#' @export
book_tex_format <- function(clean_after = TRUE, ...) {
  # Function \code{prepare_template} takes care of having template.tex
  # in place in the same directory where the files to compile are
  pdf_format(..., template = "template.tex", keep_tex = !clean_after, add_math_defs = FALSE)
}

#' Pre processing function for book_tex template
#' 
#' @param dir The directory where the files are located.
#' @export
book_tex_pre <- function(dir) {
  # Nothing to do
}

#' Post processing function for book_tex template
#' 
#' @param dir The directory where the compilation files are located.
#' @param original_dir The directory from which source files originated.
#' @export
book_tex_post <- function(dir, original_dir) {
  post.copy_pdf_to_original_dir(dir, original_dir)
}
