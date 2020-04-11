#' Ensures that a specific template's resources are deployed to the
#' specified directory
#'
#' @param dir Path to the folder where to deploy the template and its
#'     resources.
#' @param template_name Name of the template to use.
#' @return The directory where the template has been deployed (invisibly).
#' @note An librars template consists of a directory that contains a
#'     description of the template and optional additional supporting
#'     files that are provided along with the skeleton (e.g. a logo graphic).
#'
#'     The template directory should be located at \code{inst/rmarkdown/templates}
#'     inside this package.
#'     For example, a template named \code{quarterly_report} would need to
#'     provide the following files within the
#'     \code{librarstemplates/inst/rmarkdown/templates} directory:
#'
#'     \code{quarterly_report/template.yaml} \cr
#'     \code{quarterly_report/skeleton/} \cr
#'
#'     The \code{template.yaml} file should include a \code{name} and a
#'     \code{description} field.
#'
#'     Additional files can be added to the \code{skeleton} directory, for example:
#'
#'     \code{skeleton/logo.png} \cr
#'
#'     These files will automatically be copied to the deployment directory containing.
#' @examples
#' \dontrun{
#' librarstemplates::prepare_template("quarterly_report")
#' 
#' librarstemplates::prepare_template("quarterly_report", dir="/home/user/dir")
#' }
#' @export
prepare_template <- function(template_name, dir = ".") {
  package <- "librarstemplates"
  template_path <- system.file("rmarkdown", "templates", template_name, package = package)
  if (!nzchar(template_path)) {
    stop("The template '", template_name, "' was not found in the ", package, " package")
  }

  # Read the template.yaml and confirm it has the right fields
  template_meta <- get_template_info(template_name)


  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain both 'name' and 'description' fields")
  }

  # Copy all of the files in the skeleton directory
  skeleton_files <- list.files(file.path(template_path, "skeleton"), full.names = TRUE)
  to <- dir

  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f)))) {
      stop("The file '", basename(f), "' already exists")
    }
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }

  # Return the name of the file created
  invisible(dir)
}
