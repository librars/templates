#' User must specify in \code{render} or in the TOC file, the template to use.
#' From the template, the format is extracted.
#' In the format call, the template handling is present. The format function will also
#' handle which proper function to call to render the desired output type (PDF, HTML, etc.)
NULL

#' Renders an artifact
#' 
#' @param input Path to the directory containing the files to process.
#'     The directory must contain an index.md file.
#' @param template_name The name of the template to use. This is the name of the template
#'     directory in the \code{templates} folder.
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
  settings <- list(
    template_name = ifelse(!is.character(template_name) || nchar(template_name) == 0, "book_tex", template_name),
    intermediate_dir = ifelse(is.null(intermediate_dir), input, intermediate_dir),
    index_filename = ifelse(is.null(index_filename), INDEX_FILENAME, index_filename)
  )

  # Prepare environment, copy all files in there, and create intermediate folder
  intermediate_path <- prepare_env(settings$intermediate_dir) # normalized

  # Process TOC file and generate the yaml header
  config <- get_configuration(file.path(intermediate_path, settings$index_filename))
  yml_file_path <- generate_rmd_yaml_header(config, intermediate_path)

  # Merge all chapters in one file
  merged_file_path <- file.path(intermediate_path, MERGED_FILENAME)
  merge_chapters(c(yml_file_path, file.path(intermediate_path, config$rmd_files)), merged_file_path)

  # Process the merged Rmd file to transform everything necessary
  # before having markdown kick in
  process_rmd_files(merged_file_path) # Only one file (our strategy)

  # From here, rmarkdown is ready to take over
  compile_artifacts(intermediate_path, merged_file_path, template_name, input, clean_after = clean_after)

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

#' Generates the YAML header given the configuration
#' 
#' @param config The configuration object.
#' @param dir_path The path to the directory where to place the generated Rmd file.
#' @return The path to the generated file.
generate_rmd_yaml_header <- function(config, dir_path) {
  yml <- list()

  yml$title <- config$title
  yml$author <- config$author

  file_path <- file.path(dir_path, YML_HEADER_FILENAME)
  unlink(file_path)
  write_utf8(paste("---", yaml::as.yaml(yml), "---", sep = "\n"), file_path)

  file_path
}

#' Gets the configuration object out of the toc file
#' 
#' Includes:
#' - title
#' - rmd_files
#' - output_dir
#' - out_filename
#' - subtitle
#' - author
#' 
#' @param input The toc (md) file.
#' @return The configuration object.
get_configuration <- function(input) {
  if (!nzchar(input) || !file.exists(input)) {
    stop(paste("File", input, "could not be found"))
  }

  input_content <- normalize_string_newlines(get_file_content(input))

  # Mandatory fields
  matcher.title <- list(pattern = "([^#]|^)#[^#](\\s*.+)(\\n|$)", group_no = 3)
  matcher.chapter_item <- list(pattern = "-\\s*(.+)(\\n|$)", group_no = 2)
  # Optional fields
  matcher.subtitle <- list(pattern = "([^#]|^)##[^#](\\s*.+)(\\n|$)", group_no = 3)
  matcher.author <- list(pattern = "([^#]|^)###[^#]Author\\n+(.+)(\\n|$)", group_no = 3)

  config <- list()

  # Fetch mandatory fields
  config$title <- extract_tocfile_metadata(input_content, matcher.title$pattern, matcher.title$group_no)
  config$rmd_files <- paste(extract_tocfile_metadata(input_content,
                                                     matcher.chapter_item$pattern,
                                                     matcher.chapter_item$group_no),
                            FILE_EXT,
                            sep = ".") # TODO: be resilient to ext already present
  config$output_dir <- OUT_DIRNAME
  config$out_filename <- OUT_FILENAME
  # Handle optional fields
  config$subtitle <- extract_tocfile_metadata(input_content, matcher.subtitle$pattern, matcher.subtitle$group_no)
  config$author <- extract_tocfile_metadata(input_content, matcher.author$pattern, matcher.author$group_no)

  config
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

#' Merge all the chapters together
merge_chapters = function(files, dst_file_path, before = NULL, after = NULL, orig = files) {
  content = unlist(mapply(files, orig, SIMPLIFY = FALSE, FUN = function(f, o) {
    x = read_utf8(f) # TODO: use function in utils or create one specific for UTF8
    x = insert_code_chunk(x, before, after)
    c(x, "", paste0("<!--chapter:end:", o, "-->"), "")
  }))
  
  unlink(dst_file_path)
  write_utf8(content, dst_file_path)
  Sys.chmod(dst_file_path, "644")
}

#' Gets the number of matching '---'
match_dashes = function(x) grep("^---\\s*$", x)

#' Gets a vector with before, x and after
insert_code_chunk = function(x, before, after) {
  if (length(before) + length(after) == 0) {
    return(x)
  }
  if (length(x) == 0 || length(match_dashes(x[1])) == 0) {
    return(c(before, x, after))
  }

  i = match_dashes(x)
  if (length(i) < 2) {
    warning("There may be something wrong with your YAML frontmatter (no closing ---)")
    return(c(before, x, after))
  }

  # Insert `before` after the line i[2], i.e. the second ---
  c(append(x, before, i[2]), after)
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

#' Processes all Rmd files in a directory
#' 
#' The function will change the original files.
#' 
#' @param files The vector of file paths to process.
#' @return The list of the paths of processed files.
process_rmd_files <- function(files) {
  for (file in files) {
    file_content <- get_file_content(file)
    new_file_content <- process_rmd_file(file_content)
    write_file(new_file_content, file)
  }
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

#' Compiles the artifacts running preprocessor, launching bookdown and running postprocessor
#' 
#' @param dir_path The path to directory containing the resources.
#'     Rmarkdown will rely on having all template resources there.
#' @param file_path The path to the file to process. Rmarkdown will parse it.
#' @param template_name The template to use.
#' @param dstpath The path where to move the final artifacts once generated.
#' @param ... Parameters to pass to \code{launch_rmarkdown}.
compile_artifacts <- function(dir_path, file_path, template_name, dstpath, ...) {
  template_functions <- retrieve_template_functions(template_name)

  # Deploy template files
  prepare_template(template = template_name, dir = dir_path)

  # Run preprocessor
  if (!is.null(template_functions$preprocessor)) {
    print("Running preprocessor...")
    template_functions$preprocessor(dir_path)
  }

  # Launch Rmarkdown
  launch_rmarkdown(file_path, template_name, ...)

  # Run postprocessor
  if (!is.null(template_functions$postprocessor)) {
    print("Running postprocessor...")
    template_functions$postprocessor(dir_path, dstpath)
  }
}

#' Launches Rmarkdown to create the book
#' 
#' @param path The path to the file to process. Rmarkdown will parse it.
#' @param template_name The template to use.
launch_rmarkdown <- function(path, template_name, clean_after = TRUE) {
  template_info <- get_template_info(template_name)

  # Check the presence of the format function (mandatory)
  if (is.null(template_info$format)) {
    stop("The template.yaml for template", template_name, "must contain field 'format'")
  }

  output_format <- get0(template_info$format, as.environment("package:librarstemplates"))
  if (is.null(output_format)) {
    stop(paste("Output format", template_info$format, "could not be found"))
  }

  print(paste("Invoking Rmarkdown with format:", template_info$format))

  rmarkdown::render(path,
                    output_format(clean_after = clean_after),
                    clean = clean_after,
                    envir = parent.frame(),
                    encoding = "UTF-8")
}
