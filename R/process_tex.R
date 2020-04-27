#' Appends the content of a resource file to index.Rmd
#' 
#' Suited for pre-processing.
#' 
#' In this context, we need to apply the fix/hack as explained
#' in \link{https://github.com/rstudio/bookdown/issues/750}.
#' 
#' @param dir The directory where the files are located.
#' @param template_name Name of the template to.
#' @param res_filename Name of the file (ext included) whose content
#'   should be appended to index.
#' @export
pre.append_res_to_index_rmd <- function(dir, template_name, res_filename) {
  package <- "librarstemplates"

  template_path <- template.get_path(template_name, package = package) # Template folder
  if (!nzchar(template_path) || !dir_exists(template_path)) {
    stop("The template '", template_name, "' was not found in the ", package, " package")
  }

  remthm_rmd_path <- file.path(template_path, "resources", res_filename)
  if (!nzchar(remthm_rmd_path) || !file_exists(remthm_rmd_path)) {
    stop(paste("Preprocessor could not find file", remthm_rmd_path))
  }
  remthm_rmd <- get_file_content(remthm_rmd_path)

  index_rmd_path <- file.path(dir, INDEX_RMD_FILENAME)
  if (!file_exists(index_rmd_path)) {
    stop(paste("Preprocessor could not find file", index_rmd_path))
  }
  index_rmd <- get_file_content(index_rmd_path)

  new_index_rmd <- paste(index_rmd, remthm_rmd, sep = "\n\n")
  write_file(new_index_rmd, index_rmd_path)
}

#' Post processing function for book_tex template
#' 
#' Suited for post-processing.
#' 
#' @param dir The directory where the compilation files are located.
#' @param original_dir The directory from which source files originated.
#' @export
post.copy_pdf_to_original_dir <- function(dir, original_dir) {
  base_path <- file.path(dir, OUT_DIRNAME)

  to_copy <- file.path(base_path, list.files(path = base_path, pattern = "\\.pdf$"))
  file.copy(from = to_copy, to = original_dir)

  # A bug in bookdown might cause out dir not to be generated
  to_copy2 <- file.path(dir, list.files(path = dir, pattern = "\\.pdf$"))
  file.copy(from = to_copy2, to = original_dir)
}
