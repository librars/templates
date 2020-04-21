#' User must specify in \code{render} or in the TOC file, the template to use.
#' From the template, the format is extracted.
#' In the format call, the template handling is present. The format function will also
#' handle which proper function to call to render the desired output type (PDF, HTML, etc.)
NULL

#' Renders an artifact
#' 
#' @param input Path to the directory containing the files to process.
#'     The directory must contain an index.md file.
#' @param template_name The name of the template to use. This is the name appearing tin the \code{templates} folder.
#' @param output The directory path where to save the generated output.
#'     If not specirfied, it will the same as input.
#' @param intermediate_dir The directory path where the intermediate folder will be created.
#'     The intermediate folder is the directory where to
#'     place the temporary files during rendering. If not specified,
#'     it will be the same as input. The directory must exist.
#' @param clean_after A value indicating whether to clean the intermediate folder.
#' @param index_filename The name of the index file to look for. If NULL, defaults to index.md.
#' 
#' @export
render <- function(input,
                   template_name = "book_tex",
                   output = NULL,
                   intermediate_dir = NULL,
                   clean_after = TRUE,
                   index_filename = NULL) {
  config <- list(
    template_name = ifelse(!is.character(template_name) || nchar(template_name) == 0, "book_tex", template_name),
    intermediate_dir = ifelse(is.null(intermediate_dir), input, intermediate_dir),
    index_filename = ifelse(is.null(index_filename), INDEX_FILENAME, index_filename)
  )

  # Prepare environment, copy all files in there, and create intermediate folder
  intermediate_path <- prepare_env(config$intermediate_dir) # normalized

  # Process TOC file and generate _bookdown.yml and index.Rmd
  generate_config_file(file.path(intermediate_path, config$index_filename), intermediate_path, TRUE)
  generate_indexrmd_file(file.path(intermediate_path, config$index_filename), intermediate_path, TRUE)

  # Process every Rmd file in order to parse it to bookdown-format Rmd
  process_rmd_files(intermediate_path)

  # From here, bookdown is ready to take over
  compile_book(intermediate_path, template_name, input)

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
    stop(paste("Directory",
               normalized_intermediate_dir,
               "could not be found to place the intermediate directory"))
  }

  intermediate_path <- file.path(normalized_intermediate_dir, INTERMEDIATE_DIR_NAME)

  # Create the intermediate dir
  dir.create(intermediate_path)

  # Copy all required stuff inside the intermediate folder
  to_copy <- file.path(normalized_intermediate_dir,
                       list.files(path = normalized_intermediate_dir, 
                                  pattern = "\\.md$|\\.Rmd$")) # normalized
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
generate_config_file <- function(input, output, no_override = FALSE) {
  if (!file.exists(input)) {
    stop(paste("File", input, "could not be found"))
  }

  yml_filepath <- normalize_path(file.path(output, BOOKMARK_YML_FILENAME))
  if (no_override && file.exists(yml_filepath)) {
    stop(paste("Output file", yml_filepath, "already exists, will not override"))
  }

  index_content <- get_file_content(input)
  write_file(yaml::as.yaml(generate_config(index_content)), yml_filepath)

  yml_filepath
}

#' Generates the index.Rmd front matter file
#' 
#' Generates the index.Rmd file at the specified location.
#' 
#' @param input The path to the index/toc file to parse and translate.
#' @param output The path to the directory where to emit the yml file.
#' @param no_override A value indicating whether to allow overriding existing files.
#' @return The path to the generated yml file.
generate_indexrmd_file <- function(input, output, no_override = FALSE) {
  if (!file.exists(input)) {
    stop(paste("File", input, "could not be found"))
  }

  rmd_filepath <- normalize_path(file.path(output, INDEX_RMD_FILENAME))
  if (no_override && file.exists(rmd_filepath)) {
    stop(paste("Output file", rmd_filepath, "already exists, will not override"))
  }

  index_content <- get_file_content(input)

  rmd_content_frontmatter <- yaml::as.yaml(generate_indexrmd(index_content))
  rmd_content <- c("---", rmd_content_frontmatter, "---")
  writeLines(rmd_content, rmd_filepath)

  rmd_filepath
}

#' Generates the content of the _bookdown.yml file
#' 
#' As per \link{https://bookdown.org/yihui/bookdown/configuration.html#configuration}.
#' 
#' @param input The input content of the index/toc file.
#' @return The content of the _bookdown.yml file emitted as a list.
generate_config <- function(input) {
  if (!is.character(input)) {
    stop(paste("Invalid input:", input))
  }

  # Make sure to always have \n as newline
  normalized_input <- normalize_string_newlines(input)

  yml <- list()

  # Mandatory fields
  matcher.chapter_item <- list(pattern = "-\\s*(.+)(\\n|$)", group_no = 2)

  # Fetch mandatory fields
  yml$rmd_files <- paste(extract_tocfile_metadata(normalized_input,
                                                  matcher.chapter_item$pattern,
                                                  matcher.chapter_item$group_no),
                         FILE_EXT,
                         sep = ".")
  yml$output_dir <- OUT_DIRNAME
  yml$book_filename <- OUT_FILENAME

  # Add index.Rmd in rmd_files in order to show title and author
  yml$rmd_files <- c(INDEX_RMD_FILENAME, yml$rmd_files)

  yml
}

#' Generates the content of the index.Rmd file
#' 
#' @param input The input content of the index/toc file.
#' @return The content of the index.Rmd file emitted as a list.
generate_indexrmd <- function(input) {
  if (!is.character(input)) {
    stop(paste("Invalid input:", input))
  }

  # Make sure to always have \n as newline
  normalized_input <- normalize_string_newlines(input)

  yml <- list()

  # Mandatory fields
  matcher.title <- list(pattern = "([^#]|^)#[^#](\\s*.+)(\\n|$)", group_no = 3)
  # Optional fields
  matcher.subtitle <- list(pattern = "([^#]|^)##[^#](\\s*.+)(\\n|$)", group_no = 3)
  matcher.author <- list(pattern = "([^#]|^)###[^#]Author\\n+(.+)(\\n|$)", group_no = 3)

  # Fetch mandatory fields
  yml$title <- extract_tocfile_metadata(normalized_input, matcher.title$pattern, matcher.title$group_no)
  # Handle optional fields
  yml$subtitle <- extract_tocfile_metadata(normalized_input, matcher.subtitle$pattern, matcher.subtitle$group_no)
  yml$author <- extract_tocfile_metadata(normalized_input, matcher.author$pattern, matcher.author$group_no)

  yml
}

#' Extracts relevant metadata from index.md toc file
#' 
#' @param text Normalized (all newlines as \n) text content.
#' @param pattern The regex to use.
#' @param group_no The group number for extraction.
#' @return The metadata.
extract_tocfile_metadata <- function(text, pattern, group_no) {
  res <- stringr::str_extract_all(text, pattern) # list
  if (length(res) == 0) {
    return(NULL)
  }

  res <- res[[1]] # vector
  if (length(res) == 1 && identical(res[[1]], character(0))) {
    return(NULL)
  }

  extracted_matches <- stringr::str_match(res, pattern) # matrix (group_no column has the extracted values)
  metadata <- extracted_matches[, group_no] # vector

  if (length(metadata) == 0) {
    return(NULL)
  }
  if (length(metadata) == 1 && identical(metadata, character(0))) {
    return(NULL)
  }

  metadata
}

#' Parses all Rmd files into bookdown Rmd
#' 
#' The function will chanbge the original files.
#' 
#' @param path The folder where to search Rmd files.
#' @return The list of the paths of processed files.
process_rmd_files <- function(path) {
  if (!dir.exists(path)) {
    stop(paste("Directory", path, "could not be found"))
  }

  normalized_path <- normalize_path(path)

  files_to_process <- file.path(normalized_path, list.files(path = normalized_path, pattern = "\\.Rmd$"))

  for (file in files_to_process) {
    file_content <- get_file_content(file)
    new_file_content <- process_rmd_file(file_content)
    write_file(new_file_content, file)
  }

  files_to_process
}

#' Parses the content of a file and generates the Rmd that will be fed to Bookdown
#' 
#' @param input The content of the file to parse and translate.
#' @param ... Parameters to pass to \code{rmdconvert.scan}.
#' @return The modified content to replace.
process_rmd_file <- function(input, ...) {
  # Make sure to always have \n as newline
  normalized_input <- normalize_string_newlines(input)

  processed_file <- rmdconvert.scan(normalized_input, ...)
  processed_file$output
}

#' Retrieves the format, preprocessing and postprocessing functions for a template
#' 
#' @param template_name The name of the template.
#' @returns A list with function objects.
retrieve_template_functions <- function(template_name) {
  template_info <- get_template_info(template_name)

  get_function <- function(fun_name) {
    get_pkg_function(fun_name)
  }

  list(
    format = get_function(template_info$format),
    preprocessor = get_function(template_info$preprocessor),
    postprocessor = get_function(template_info$postprocessor)
  )
}

#' The default format
def_format <- "book_tex_format"

#' Returns the proper function call to the desired librars format
#' 
#' @param format The name of the desired format function.
#' @return The proper call to the desired format.
get_format_call <- function(format) {
  ifelse (grepl("librarstemplates::", format),
          paste(format, "()", sep = ""),
          paste("librarstemplates::", format, "()", sep = ""))
}

#' Compiles the book running preprocessor, launching bookdown and running postprocessor
#' 
#' @param path The path to the directory where files are. This is the working directory where
#'     bookdown will run and it expects to have all Rmd files and template support files too.
#' @param template_name The template to use.
#' @param dstpath The path where to move the final artifacts once generated.
compile_book <- function(path, template_name, dstpath) {
  template_functions <- retrieve_template_functions(template_name)

  # Deploy template files
  prepare_template(template = template_name, dir = path)

  # Run preprocessor
  if (!is.null(template_functions$preprocessor)) {
    print("Running preprocessor...")
    template_functions$preprocessor(path)
  }

  # Launch Bookdown
  launch_bookdown(path, template_name)

  # Run postprocessor
  if (!is.null(template_functions$postprocessor)) {
    print("Running postprocessor...")
    template_functions$postprocessor(path, dstpath)
  }
}

#' Launches bookdown to create the book
#' 
#' @param path The path to the directory where files are. This is the working directory where
#'     bookdown will run and it expects to have all Rmd files and template support files too.
#' @param template_name The template to use.
launch_bookdown <- function(path, template_name) {
  template_info <- get_template_info(template_name)

  # Check the presence of the format function (mandatory)
  if (is.null(template_info$format)) {
    stop("The template.yaml for template", template_name, "must contain field 'format'")
  }

  template_format_invocation <- get_format_call(template_info$format)
  quiet <- FALSE
  normalized_path <- normalize_path(path)

  # Bookdown does not properly support passing paths to folders on which running,
  # therefore we need to create a new sessions and run there
  cmd = paste(sprintf("setwd('%s')", normalized_path),
              sprintf("bookdown::render_book('index.Rmd', %s, quiet = %s)", template_format_invocation, quiet), 
              sep = ";")

  print(paste("Invoking bookdown:", cmd, "..."))

  Rscript(c("-e", shQuote(cmd)))
}
