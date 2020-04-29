#' Convert R Markdown to a PDF book
#'
#' Convert R Markdown files to PDF after resolving the special tokens of
#' \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
#' commands.
#'
#' This function is based on \code{rmarkdown::\link{pdf_document}} (by default)
#' with better default arguments. You can also change the default format to
#' other LaTeX/PDF format functions using the \code{base_format} argument.
#'
#' The global R option \code{bookdown.post.latex} can be set to a function to
#' post-process the LaTeX output. This function takes the character vector of
#' the LaTeX output as its input argument, and should return a character vector
#' to be written to the \file{.tex} output file. This gives you full power to
#' post-process the LaTeX output.
#' @param toc,number_sections,fig_caption,pandoc_args See
#'   \code{rmarkdown::\link{pdf_document}}, or the documentation of the
#'   \code{base_format} function.
#' @param ... Other arguments to be passed to \code{base_format}.
#' @param base_format An output format function to be used as the base format.
#' @param toc_unnumbered Whether to add unnumberred headers to the table of
#'   contents.
#' @param toc_appendix Whether to add the appendix to the table of contents.
#' @param toc_bib Whether to add the bibliography section to the table of
#'   contents.
#' @param quote_footer If a character vector of length 2 and the quote footer
#'   starts with three dashes (\samp{---}), \code{quote_footer[1]} will be
#'   prepended to the footer, and \code{quote_footer[2]} will be appended; if
#'   \code{NULL}, the quote footer will not be processed.
#' @param highlight_bw Whether to convert colors for syntax highlighting to
#'   black-and-white (grayscale).
#' @note This output format can only be used with \code{\link{render_book}()}.
#' @export
pdf_format = function(toc = TRUE,
                      number_sections = TRUE,
                      fig_caption = TRUE,
                      pandoc_args = NULL,
                      ...,
                      base_format = rmarkdown::pdf_document,
                      toc_unnumbered = TRUE,
                      toc_appendix = FALSE,
                      toc_bib = FALSE,
                      quote_footer = NULL,
                      highlight_bw = FALSE,
                      add_math_defs = TRUE,
                      to_pdf = TRUE
                     ) {
  config <- get_base_format(base_format, list(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    pandoc_args = pandoc_args2(pandoc_args), ...))

  # Ensure proper extension: Pandoc to output Latex
  config$pandoc$ext = ".tex"

  # Post processor (after Pandoc's compile, output is Latex)
  post <- config$post_processor  # In case a post processor have been defined
  # metadata: settings and options
  # output: the path to file being processed
  config$post_processor <- function(metadata, input, output, clean, verbose) {
    if (is.function(post)) {
      output <- post(metadata, input, output, clean, verbose)
    }

    # Pipeline
    f <- with_ext(output, ".tex")
    x <- resolve_refs_latex(read_utf8(f))
    x <- resolve_ref_links_latex(x)
    x <- restore_appendix_latex(x, toc_appendix)
    if (!toc_unnumbered) x <- remove_toc_items(x)
    if (toc_bib) x <- add_toc_bib(x)
    x <- restore_block2(x, !number_sections, add_math_defs)
    if (!is.null(quote_footer)) {
      if (length(quote_footer) != 2 || !is.character(quote_footer)) {
        warning("The 'quote_footer' argument should be a character vector of length 2")
      } else {
        x <- process_quote_latex(x, quote_footer)
      }
    }
    if (highlight_bw) x <- highlight_grayscale_latex(x)

    # Rewrite new content
    write_utf8(x, f)

    # Compile Latex into PDF
    if (to_pdf) {
      tinytex::latexmk(f, config$pandoc$latex_engine,
                       if ("--biblatex" %in% config$pandoc$args) "biber" else "bibtex")
    }

    output <- with_ext(output, ".pdf")
    o <- opts$get("output_dir")
    keep_tex <- isTRUE(config$pandoc$keep_tex)
    if (!keep_tex) {
      file.remove(f)
    }
    if (is.null(o)) {
      return(output)
    }

    output2 <- file.path(o, output)
    file.rename(output, output2)
    if (keep_tex) {
      file.rename(f, file.path(o, f))
    }
    output2
  }

  # Pre processor (before Pandoc's compile)
  pre = config$pre_processor
  config$pre_processor = function(metadata, input, runtime, knit_meta, files_dir, output_dir) {
    pre_result <- if (is.function(pre)) pre(metadata, input, runtime, knit_meta, files_dir, output_dir)

    # Always enable tables (use packages booktabs, longtable, ...)
    c(pre_result, "--variable", "tables=yes", "--standalone",
      if (rmarkdown::pandoc_available("2.7.1")) "-Mhas-frontmatter=false"
    )
  }

  config = set_opts_knit(config)
  config
}

# set some internal knitr options
set_opts_knit = function(config) {
  # use labels of the form (\#label) in knitr
  config$knitr$opts_knit$bookdown.internal.label = TRUE
  # when the output is LaTeX, force LaTeX tables instead of default Pandoc tables
  # http://tex.stackexchange.com/q/276699/9128
  config$knitr$opts_knit$kable.force.latex = TRUE
  config
}

get_base_format = function(format, options = list()) {
  if (is.character(format)) format = eval(parse(text = format))
  if (!is.function(format)) stop('The output format must be a function')
  # make sure named elements in `options` have corresponding named arguments in
  # the format function, unless the function has the ... argument
  nms = names(formals(format))
  if (!('...' %in% nms)) options = options[names(options) %in% c(nms, '')]
  do.call(format, options)
}

#' Resolve references, expects vector of lines
resolve_refs_latex = function(x, use_amsmath = TRUE) {
  eq_begins <- get_eq_block_begin_end_lines(x, TRUE) # Indices
  eq_ends <- get_eq_block_begin_end_lines(x, FALSE) # Indices
  if (identical(eq_begins, eq_ends)) {
    stop("Number of openings and endings for equation environments mismatch")
  }
  labeldef_pattern <- "^(.*)(\\(label: (.*)\\))(.*)$"
  # Indices where equation label definitions are
  label_in_eq_block_matches_i <- get_label_matches_in_eq_block(x, labeldef_pattern, eq_begins, eq_ends)
  # Equation label definition values
  label_in_eq_block_matches <- gsub(labeldef_pattern, "\\3", x[label_in_eq_block_matches_i])

  # References to labels: '[...](label: ...)' where label is
  # defined inside 'begin{equation}...\end{equation}'
  # We need to proceed line by line as we need to target different expressions
  x2 <- c()
  labelref_pattern <- "^(.*)(\\[.*\\]\\(label: (.*)\\))(.*)$"
  labelref_repl <- "\\1\\\\%s\\{\\3\\}\\4"
  labelref_repl_normal <- sprintf(labelref_repl, "ref")
  labelref_repl_amsmatheq <- sprintf(labelref_repl, "eqref")
  for (i in 1:length(x)) {
    if (length(grep(labelref_pattern, x[i]))) {
      label_name <- gsub(labelref_pattern, "\\3", x[i])
      if (label_name %in% label_in_eq_block_matches) {
        # The label this reference points to is an equation label
        x2 <- c(x2, gsub(labelref_pattern, if (use_amsmath) labelref_repl_amsmatheq else labelref_repl_normal, x[i]))
      } else {
        # The label this reference points to is a normal label
        x2 <- c(x2, gsub(labelref_pattern, labelref_repl_normal, x[i]))
      }
    } else {
      # The line does not contain references to labels
      x2 <- c(x2, x[i])
    }
  }

  # Label definitions: '(label: ...)'
  # Must be done after label references as the syntax for a reference
  # include the syntax of definitions
  x2 <- gsub(labeldef_pattern, "\\1\\\\label\\{\\3\\}\\4", x2)

  x2
}

#' Gets the indices where labels are inside equation blocks
get_label_matches_in_eq_block <- function(x, label_pattern, eq_begins, eq_ends) {
  i <- grep(label_pattern, x) # Indices where all label defs are
  # Check with eq begins and ends and return only the indices in blocks
  i[is_inside_block(i, eq_begins, eq_ends)]
}

#' Gets TRUE when the index is inside a block whose
#' opening and ending indices are provided, i is a vector
is_inside_block <- function(i, begins, ends) {
  unlist(lapply(i, FUN = function(k) {
    for (j in 1:length(begins)) {
      if (begins[j] < k && k < ends[j]) {
        return(TRUE)
      }
    }
    FALSE
  }))
}

#' Get the lines where an equation block starts or terminates
get_eq_block_begin_end_lines <- function(x, begin = TRUE) {
  if (begin) {
    grep("^.*\\\\begin\\{equation\\*?\\}.*$", x)
  } else {
    grep("^.*\\\\end\\{equation\\*?\\}.*$", x)
  }
}

resolve_ref_links_latex = function(x) {
  res = parse_ref_links(x, '^%s (.+)$')
  if (is.null(res)) return(x)
  x = res$content; txts = res$txts; i = res$matches
  # text for a tag may be wrapped into multiple lines; collect them until the
  # empty line
  for (j in seq_along(i)) {
    k = 1
    while (x[i[j] + k] != '') {
      txts[j] = paste(txts[j], x[i[j] + k], sep = '\n')
      x[i[j] + k] = ''
      k = k + 1
    }
  }
  restore_ref_links(x, '(?<!\\\\texttt{)%s', res$tags, txts, FALSE)
}

reg_ref_links = '(\\(ref:[-/[:alnum:]]+\\))'

parse_ref_links = function(x, regexp) {
  r = sprintf(regexp, reg_ref_links)
  if (length(i <- grep(r, x)) == 0) return()
  tags = gsub(r, '\\1', x[i])
  txts = gsub(r, '\\2', x[i])
  if (any(k <- duplicated(tags))) {
    warning('Possibly duplicated text reference labels: ', paste(tags[k], collapse = ', '))
    k = !k
    tags = tags[k]
    txts = txts[k]
    i = i[k]
  }
  x[i] = ''
  list(content = x, tags = tags, txts = txts, matches = i)
}

restore_ref_links = function(x, regexp, tags, txts, alt = TRUE) {
  r = sprintf(regexp, reg_ref_links)
  m = gregexpr(r, x, perl = TRUE)
  tagm = regmatches(x, m)
  for (i in seq_along(tagm)) {
    tag = tagm[[i]]
    if (length(tag) == 0) next
    k = match(tag, tags)
    tag[!is.na(k)] = txts[na.omit(k)]
    if (alt && is_img_line(x[i])) tag = strip_html(tag)
    tagm[[i]] = tag
  }
  regmatches(x, m) = tagm
  x
}

restore_appendix_latex = function(x, toc = FALSE) {
  r = '^\\\\(chapter|section)\\*\\{\\(APPENDIX\\) .*'
  i = find_appendix_line(r, x)
  if (length(i) == 0) return(x)
  level = gsub(r, '\\1', x[i])
  brace = grepl('}}$', x[i])
  x[i] = '\\appendix'
  if (toc) x[i] = paste(
    x[i], sprintf('\\addcontentsline{toc}{%s}{\\appendixname}', level)
  )
  if (brace) x[i] = paste0(x[i], '}')  # pandoc 2.0
  if (grepl('^\\\\addcontentsline', x[i + 1])) x[i + 1] = ''
  x
}

find_appendix_line = function(r, x) {
  i = grep(r, x)
  if (length(i) > 1) stop('You must not have more than one appendix title')
  i
}

remove_toc_items = function(x) {
  r = '^\\\\addcontentsline\\{toc\\}\\{(part|chapter|section|subsection|subsubsection)\\}\\{.+\\}$'
  x[grep(r, x)] = ''
  x
}

add_toc_bib = function(x) {
  r = "^\\\\bibliography\\{.+\\}$"
  i = grep(r, x)
  if (length(i) == 0) {
    return(x)
  }

  i = i[1]
  level = if (length(grep("^\\\\chapter\\*?\\{", x))) "chapter" else "section"
  x[i] = sprintf("%s\n\\addcontentsline{toc}{%s}{\\bibname}", x[i], level)
  x
}

restore_block2 = function(x, global = FALSE, add_math_defs = TRUE) {
  i = grep('^\\\\begin\\{document\\}', x)[1]
  if (is.na(i)) {
    return(x)
  }

  if (length(grep('\\\\(Begin|End)KnitrBlock', tail(x, -i)))) {
    x = append(x, '\\let\\BeginKnitrBlock\\begin \\let\\EndKnitrBlock\\end', i - 1)
  }
  
  if (length(grep(sprintf('^\\\\BeginKnitrBlock\\{(%s)\\}', paste(all_math_env, collapse = '|')), x)) &&
      length(grep('^\\s*\\\\newtheorem\\{theorem\\}', head(x, i))) == 0 &&
      add_math_defs) {
    theorem_defs = sprintf(
      '%s\\newtheorem{%s}{%s}%s', theorem_style(names(theorem_abbr)), names(theorem_abbr),
      str_trim(vapply(theorem_abbr, label_prefix, character(1), USE.NAMES = FALSE)),
      if (global) '' else {
        if (length(grep('^\\\\chapter[*]?', x))) '[chapter]' else '[section]'
      }
    )
    # the proof environment has already been defined by amsthm
    proof_envs = setdiff(names(label_names_math2), 'proof')
    proof_defs = sprintf(
      '%s\\newtheorem*{%s}{%s}', theorem_style(proof_envs), proof_envs,
      gsub('^\\s+|[.]\\s*$', '', vapply(proof_envs, label_prefix, character(1), label_names_math2))
    )
    x = append(x, c('\\usepackage{amsthm}', theorem_defs, proof_defs), i - 1)
  }
  # remove the empty lines around the block2 environments
  i3 = if (length(i1 <- grep('^\\\\BeginKnitrBlock\\{', x))) (i1 + 1)[x[i1 + 1] == '']
  i3 = c(i3, if (length(i2 <- grep('^\\\\EndKnitrBlock\\{', x))) (i2 - 1)[x[i2 - 1] == ''])
  if (length(i3)) x = x[-i3]

  r = '^(.*\\\\BeginKnitrBlock\\{[^}]+\\})(\\\\iffalse\\{-)([-0-9]+)(-\\}\\\\fi\\{\\})(.*)$'
  if (length(i <- grep(r, x)) == 0) return(x)
  opts = sapply(strsplit(gsub(r, '\\3', x[i]), '-'), function(z) {
    intToUtf8(as.integer(z))
  }, USE.NAMES = FALSE)
  x[i] = paste0(gsub(r, '\\1', x[i]), opts, gsub(r, '\\5', x[i]))
  x
}

style_definition = c('definition', 'example', 'exercise')
style_remark = c('remark')
# which styles of theorem environments to use
theorem_style = function(env) {
  styles = character(length(env))
  styles[env %in% style_definition] = '\\theoremstyle{definition}\n'
  styles[env %in% style_remark] = '\\theoremstyle{remark}\n'
  styles
}

process_quote_latex = function(x, commands) {
  for (i in grep('^\\\\end\\{quote\\}$', x)) {
    i1 = NULL; i2 = i - 1
    k = 1
    while (k < i) {
      xk = x[i - k]
      if (grepl('^---.+', xk)) {
        i1 = i - k
        break
      }
      if (xk == '' || grepl('^\\\\begin', xk)) break
      k = k + 1
    }
    if (is.null(i1)) next
    x[i1] = paste0(commands[1], x[i1])
    x[i2] = paste0(x[i2], commands[2])
  }
  x
}

# \newenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
# \newcommand{\KeywordTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{\textbf{{#1}}}}
# \newcommand{\DataTypeTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{{#1}}}
# ...
highlight_grayscale_latex = function(x) {
  i1 = grep('^\\\\newenvironment\\{Shaded\\}', x)
  if (length(i1) == 0) return(x)
  i1 = i1[1]
  r1 = '^\\\\newcommand\\{\\\\[a-zA-Z]+\\}\\[1]\\{.*\\{#1\\}.*\\}$'
  r2 = '^(.*?)([.0-9]+,[.0-9]+,[.0-9]+)(.*)$'
  i = i1 + 1
  while (grepl('^\\\\newcommand\\{.+\\}$', x[i])) {
    if (grepl(r1, x[i]) && grepl(r2, x[i])) {
      col = as.numeric(strsplit(gsub(r2, '\\2', x[i]), ',')[[1]])
      x[i] = gsub(
        r2, paste0('\\1', paste(round(rgb2gray(col), 2), collapse = ','), '\\3'),
        x[i]
      )
    }
    i = i + 1
  }
  x
}

# https://en.wikipedia.org/wiki/Grayscale
rgb2gray = function(x, maxColorValue = 1) {
  rep(sum(c(.2126, .7152, .0722) * x/maxColorValue), 3)
}
