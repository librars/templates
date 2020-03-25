#' Wrappers for rmarkdown functions.

#' @export
output_format <- function(keep_md = FALSE,
                          clean_supporting = TRUE,
                          df_print = NULL,
                          pre_knit = NULL,
                          post_knit = NULL,
                          pre_processor = NULL,
                          intermediates_generator = NULL,
                          post_processor = NULL,
                          on_exit = NULL,
                          base_format = NULL) {
  # Call RMarkdown
  rmarkdown::output_format(knitr = NULL,
                           pandoc = NULL,
                           keep_md = keep_md,
                           clean_supporting = clean_supporting,
                           df_print = df_print,
                           pre_knit = pre_knit,
                           post_knit = post_knit,
                           pre_processor = pre_processor,
                           intermediates_generator = intermediates_generator,
                           post_processor = post_processor,
                           on_exit = on_exit,
                           base_format = base_format)
}
