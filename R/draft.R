#' Locates the template and deploys the 'draft' content folder
#' into the specified path
#' 
#' @param template_name Name of the template to use.
#' @param dir Path to the folder where to deploy the draft resources.
#' @return The directory where the files have been deployed (invisibly).
#' @note A template might not provide a draft.
#' @examples
#' \dontrun{
#' librarstemplates::draft("quarterly_report")
#' 
#' librarstemplates::draft("quarterly_report", dir="/home/user/dir")
#' }
#' @export
draft <- function(template_name, dir = ".") {
  package <- "librarstemplates"
  template_path <- system.file("rmarkdown", "templates", template_name, package = package)
  if (!nzchar(template_path)) {
    stop("The template '", template_name, "' was not found in the ", package, " package")
  }

  if (!dir.exists(file.path(template_path, "draft"))) {
    stop("Template", template_name, "does not provide a draft")
  }

  # Copy all of the files in the 'draft' directory
  draft_files <- list.files(file.path(template_path, "draft"), full.names = TRUE)
  to <- dir

  for (f in draft_files) {
    if (file.exists(file.path(to, basename(f)))) {
      stop("The file '", basename(f), "' already exists")
    }
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }

  # Return the directory
  invisible(dir)
}