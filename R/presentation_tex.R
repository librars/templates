#' Format function for presentation_tex template
#'
#' This format renders a PDF and selects the template available
#' in the folder at the moment of rendering.
#'
#' @inheritParams bookdown::pdf_book
#' @param ... Arguments to \code{bookdown::pdf_book}.
#' @param clean_after A value indicating whether to clean after or not.
#' @return R Markdown output format to pass to
#'   \code{\link[bookdown::render_book]{render_book}}.
#' @examples
#' \dontrun{
#' bookdown::render_book("MyArticle.Rmd", librarstemplates::presentation_tex_format())
#' }
#'
#' @export
presentation_tex_format <- function(clean_after = TRUE, ...) {
  # Function \code{prepare_template} takes care of having template.tex
  # in place in the same directory where the files to compile are
  rmarkdown::beamer_presentation(..., template = "template.tex",
                                 keep_tex = !clean_after, keep_md = !clean_after)
}

#' Pre processing function for presentation_tex template
#' 
#' In this context, we need to apply the fix/hack as explained
#' in \link{https://github.com/rstudio/bookdown/issues/750}.
#' 
#' @param dir The directory where the files are located.
#' @export
presentation_tex_pre <- function(dir) {
  pre.append_res_to_index_rmd(dir, "presentation_tex", "rem_thm_defs.Rmd")
}

#' Post processing function for presentation_tex template
#' 
#' @param dir The directory where the compilation files are located.
#' @param original_dir The directory from which source files originated.
#' @export
presentation_tex_post <- function(dir, original_dir) {
  post.copy_pdf_to_original_dir(dir, original_dir)
}
