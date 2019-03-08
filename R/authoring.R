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

#' Deploys resources of a template and the template itself into a
#' new directory where it will be possible to successfully compile the book.
#'
#' @param template The name of the template file to deploy. This is just the
#'   template directory name or the full path to the directory.
#'   The template directory is expected to be available inside
#'   \code{rmarkdown/templates}.
#' @param package (Optional) Name of package where the template is located,
#'   if not provided, the current package is used.
#' @param dir_name (Optional) The name of the destination folder.
#' @param file_name (Optional) The name of the core template file. File
#'   'skeleton.Rmd' will be renamed to this, otherwise the name will remain
#'   the original.
#'
#' @examples
#' \dontrun{
#' librarstemplates::deploy_template(template="/opt/rmd/templates/report")
#'
#' librarstemplates::deploy_template(template="report", package="mypackage")
#'
#' librarstemplates::deploy_template(template="report",
#'   package="mypackage", dir_name="myreport", file_name="report.Rmd")
#' }
#'
#' @export
deploy_template <- function(template,
                            package = NULL,
                            dir_name = NULL,
                            file_name = NULL) {
  # Resolve the package and get the template directory
  if (!is.null(package)) {
    template_path = system.file("rmarkdown", "templates",
                                template, package = package)
    if (!nzchar(template_path)) {
      stop("Template '", template, "' was not found in package '", package, "'")
    }
  } else {
    template_path <- template
  }

  # Create a new directory
  if (!is.null(dir_name)) {
    dir_name <- template
  }
  if (dir_exists(dir_name)) {
    stop("Directory '", dir_name,
         "' already exists! Remove it, if you wish to continue.")
  }
  dir.create(dir_name)

  # Copy all the files in the skeleton directory
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  for (f in skeleton_files) {
    if (file.exists(file.path(dir_name, basename(f)))) {
      stop("The file '", basename(f), "' already exists")
    }
    file.copy(from = f, to = dir_name, overwrite = FALSE, recursive = TRUE)
  }

  # rename the core template file
  if (!is.null(file_name)) {
    file.rename(file.path(dir_name, "skeleton.Rmd"), file_name)
  }

  # return the name of the file created
  invisible(file_name)
}
