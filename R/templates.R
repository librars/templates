#' Fetches the specified template's template.yaml and returns it parsed
#' 
#' @param template_name The name of the template to fetch.
#' @export
get_template_info <- function(template_name) {
  package <- "librarstemplates"
  template_path <- system.file("rmarkdown", "templates", template_name, package = package)
  if (!nzchar(template_path)) {
    stop("The template '", template_name, "' was not found in the ", package, " package")
  }

  # Read the template.yaml and confirm it has the right fields
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", template_name, "'")
  }

  yaml_load_file(template_yaml)
}

#' Gets a list of all available templates
#' 
#' @param as.json A value indicating whether JSON output should be emitted.
#' @export
get_template_list <- function(as.json = FALSE) {
  dirs_list <- list_template_dirs()
  res <- c()

  for (dir in dirs_list) {
    splitted <- strsplit(dir, "/")[[1]]
    res <- c(res, splitted[length(splitted)])
  }

  if (isTRUE(as.json)) {
    return(jsonlite::toJSON(list(templates = res)))
  }

  res
}

#' Lists the template directories that are available for consumption.
list_template_dirs <- function(pkg = "librarstemplates") {
  # Check to see if the package includes a template folder
  template_folder <- system.file("rmarkdown", "templates", package = pkg)

  list <- c()
  if (dir_exists(template_folder)) {
    template_dirs <- list.dirs(path = template_folder, recursive = FALSE)

    for (dir in template_dirs) {
      list <- c(list, dir)
    }
  }

  list
}
