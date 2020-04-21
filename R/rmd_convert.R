#' Functions to convert LibRArs' markdown into Bookdown's markdown.
#' Assumption: input stirngs all with \n newlines.
NULL

### Utilities ###

s <- function(...) {
  paste(..., sep = "")
}

nl <- function(...) {
  paste(..., sep = "\n")
}

create_log <- function() {
  log <- matrix(ncol = 4)
  colnames(log) <- c("Pattern name", "Match text", "Replaced text", "Details")
  log
}

tidy_log <- function(log) {
  log[!is.na(log[,1]),] # Get rid of NA rows
}

### Generic ###

#' A newline or the start of the string (1 group)
regex.line_start <- "(^|\\n)"
#' A space or a tab
regex.spacing <- "[ \t]"
#' A space or a tab at leats once
regex.spacing.mandatory <- s(regex.spacing, "+")
#' A space or a tab zero or more times
regex.spacing.optional <- s(regex.spacing, "*")
#' Any character or a single newline (a newline immediately followed by a non-newline)
#' (2 groups)
regex.any_char_incl_single_newline <- "(.|(\\n[^\\n]))"
#' Any character or a single newline zero or more times (3 groups)
regex.env_body <- s("(", regex.any_char_incl_single_newline, "*)")
#' Any number of newline followed by a # or two blank lines (3 newlines) (3 groups)
regex.env_end <- "((\\n+#)|(\\n\\n\\n))"

#' Scans the source string and transforms it against all constructs
#' 
#' Important: the order though which target constructs are scanned
#' is important.
#' 
#' @param rmd_src The source Rmd string.
#' @param debug.info When \code{TRUE} it will instruct the function to log debug info.
#' @return The source string with all constructs replaced.
rmdconvert.scan <- function(rmd_src, debug.info = FALSE) {
  rmd_src_processed <- rmd_src
  all_log <- create_log()

  # In sequence, run al scanners (order is important)
  # All processors must exhibit same interface: (text, debug.info)
  processors <- c(
    rmdconvert.scan.envs
  ) # list

  for (processor in processors) {
    processor_res <- processor(rmd_src_processed, debug.info)
    rmd_src_processed <- processor_res$output
    if (debug.info) {
      all_log <- rbind(all_log, processor_res$debug_info)
    }
  }

  # Return the final overall processed result
  list(output = rmd_src_processed, debug_info = tidy_log(all_log))
}

### Environemnts ###

#' Sharp followed by bang, then optional spacing, then env name (label with no spaces),
#' then mandatory spacing, then title (2 groups)
regex.env_head_with_title <- "#\\![ \t]*([^ \\n\\t]+)[ \t]+(.+)"
#' Sharp followed by bang, then optional spacing, then env name (label with no spaces),
#' (1 group)
regex.env_head_no_title <- "#\\![ \t]*([^ \\n\\t]+)"

regex.env.with_title <- s(regex.line_start,
                          regex.spacing.optional,
                          regex.env_head_with_title,
                          "\\n",
                          regex.env_body,
                          regex.env_end)
regex.env.no_title <- s(regex.line_start,
                        regex.spacing.optional,
                        regex.env_head_no_title,
                        "\\n",
                        regex.env_body,
                        regex.env_end)

#' Scans the source string and transforms it against all environment constructs
#' 
#' Use \code{rmdconvert.scan.envs(text, TRUE)} to debug the progression of
#' replacements.
#' Using the table you get from the previous command, use
#' \code{stringr::str_match(<Match text>, <pattern>)} where the pattern can be
#' retrieved by using the variable name associated to the 'Pattern name' column
#' of the table.
#' 
#' @param rmd_src The source Rmd string.
#' @param debug.info When \code{TRUE} it will instruct the function to log debug info.
#' @return The source string with all constructs replaced.
rmdconvert.scan.envs <- function(rmd_src, debug.info = FALSE) {
  patterns <- list(
    list(name = "With title", regex = regex.env.with_title,
         g_before = 2, g_name = 3, g_title = 4, g_body = 5, g_after = 8),
    list(name = "No title", regex = regex.env.no_title,
         g_before = 2, g_name = 3, g_title = NULL, g_body = 4, g_after = 6)
  )

  rmd_out <- rmd_src
  log <- create_log()

  for (pattern in patterns) { # pattern is a list
    replacer <- function(match) {
      groups <- stringr::str_match(match, pattern$regex) # matrix
      # Each of the groups, if not matched, will give NA
      before <- groups[, pattern$g_before]
      name <- groups[, pattern$g_name]
      title <- ifelse(is.null(pattern$g_title), NA, groups[, pattern$g_title])
      body <- groups[, pattern$g_body]
      after <- groups[, pattern$g_after]
      result <- rmsconvert.create.env(name, body, title, before, after)
      # Log operation
      if (debug.info) {
        log <<- rbind(log, c(pattern$name, match, result, 
                      sprintf("before='%s';name='%s';title='%s';body*='%s';after='%s'",
                              before, name, title, str_excerpt(body), after)))
      }
      # Return the constructed output to replace
      result
    }
    rmd_out <- stringr::str_replace_all(rmd_out, pattern$regex, replacer)
  }

  list(output = rmd_out, debug_info = tidy_log(log))
}

#' Creates the transformed output for an environment
#' 
#' @param name The name of the environment.
#' @param body The content of the environment.
#' @param title The title of the environment.
#' @param before The before content.
#' @param after The after content.
rmsconvert.create.env <- function(name, body, title = NA, before = NA, after = NA) {
  cbefore <- ifelse(is.na(before), "", before)
  cafter <- ifelse(is.na(after), "", after)
  label <- ifelse(is.na(title),
                  sprintf("{%s}", name),
                  sprintf("{%s, name=\"%s\"}", name, title))

  nl(s(cbefore, "```", label),
     body,
     s("```", cafter))
}
