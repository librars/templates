#' Renders an artifact
#' 
#' @param input Path to the directory containing the files to process.
#'     The directory must contain an index.md file.
#' @param output The directory path where to save the generated output.
#'     If not specirfied, it will the same as input.
#' @param intermediate_dir The intermediate directory path where to
#'     place the temporary files during rendering. If not specified,
#'     it will be the same as input. The directory must exist.
#' @param clean_after A value indicating whether to clean the intermediate folder.
#' @param index_filename The name of the index file to look for. If NULL, defaults to index.md.
#' 
#' @export
render <- function(input, output = NULL, intermediate_dir = NULL, clean_after = TRUE, index_filename = NULL) {
  config <- list(
    intermediate_dir = ifelse(is.null(intermediate_dir), input, intermediate_dir),
    index_filename = ifelse(is.null(index_filename), INDEX_FILENAME, index_filename)
  )

  # Prepare environment and create intermediate folder
  intermediate_path <- prepare_env(config$intermediate_dir) # normalized

  # Process TOC file
  generate_config(file.path(intermediate_path, config$index_filename), intermediate_path, TRUE)

  # Clean at the end
  if (clean_after) {
    clean_env(intermediate_path)
  }
}

#' Creates the intermediate dir and all needed to carry on
#' 
#' @param intermediate_dir Path where to create the intermediate dir
#'     (does not include the name of the intermediate dir).
#' @param no_override A value indicating whether to allow overriding existing files.
#' @return The intermediate path.
prepare_env <- function(intermediate_dir, no_override = FALSE) {
  # Make sure to normalize paths
  normalized_intermediate_dir <- normalize_path(intermediate_dir)

  if (!dir.exists(normalized_intermediate_dir)) {
    stop(paste("Directory", normalized_intermediate_dir, "could not be found to place the intermediate directory"))
  }

  intermediate_path <- file.path(normalized_intermediate_dir, INTERMEDIATE_DIR_NAME)

  # Create the intermediate dir
  dir.create(intermediate_path)

  # Copy all required stuff inside the intermediate folder
  to_copy <- file.path(normalized_intermediate_dir,
                       list.files(path = normalized_intermediate_dir, pattern = "\\.md$|\\.Rmd$")) # normalized
  file.copy(from = to_copy, to = intermediate_path)

  intermediate_path
}

#' Removes the intermediate directory and all its contents
clean_env <- function(intermediate_path) {
  unlink(intermediate_path, recursive = TRUE)
}

#' Generates the _bookdown.yml configuration file
#' 
#' Generates the _bookdown.yml file at the specified location.
#' 
#' @param input The path to the index/toc file to parse and translate.
#' @param output The path to the directory where to emit the yml file.
#' @param no_override A value indicating whether to allow overriding existing files.
#' @return The path to the generated yml file.
generate_config <- function(input, output, no_override = FALSE) {
  if (!file.exists(input)) {
    stop(paste("File", input, "could not be found"))
  }

  if (no_override && file.exists(output)) {
    stop(paste("Output file", output, "already exists, will not override"))
  }

  yml_filepath <- normalize_path(file.path(output, BOOKMARK_YML_FILENAME))
  index_content <- get_file_content(input)
  writeLines(generate_config(index_content), yml_filepath)

  yml_filepath
}

#' Generates the content of the _bookdown.yml file
#' 
#' @param input The input content of the index/toc file.
#' @return The content of the _bookdown.yml file.
generate_config <- function(input) {
  if (!is.character(input)) {
    stop(paste("Invalid input:", input))
  }

  # Make sure to always have \n as newline
  normalized_input <- normalize_string_newlines(input)

  yml <- list()

  # Mandatory fields
  matcher.title <- "([^#]|^)#[^#](\\s*.+)\\n"
  matcher.chapter_item <- "-\\s*(.+)\\n"
  # Optional fields
  matcher.subtitle <- "([^#]|^)##[^#](\\s*.+)\\n"
  matcher.author <- "([^#]|^)###[^#]Author\\n+(.+)\\n"

  extract <- function(pattern) {
    res <- stringr::str_extract_all(normalized_input, pattern) # list
    if (length(res) == 0) {
      return(NULL)
    }

    res <- res[[1]] # vector
    if (length(res) == 1 && identical(res[[1]], character(0))) {
      return(NULL)
    }

    extracted_matches <- stringr::str_match(res, pattern) # matrix (second column has the extracted values)
    extracted_matches[, 2] # vector
  }

  # Fetch mandatory fields
  yml$title <- extract(matcher.title)

  yaml::as.yaml(yml)
}

#' Parses a file and generates the Rmd that will be fed to bookdown
#' 
#' @param input The path to the file to parse and translate.
#' @param output The path to the directory where to emit the generated file.
process_file <- function(input, output) {

}
