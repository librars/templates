#' Registers an engine in knitr
register_knitr_eng <- function(envs, engine) {
  knitr::knit_engines$set(setNames(lapply(envs, function(env) {
    function(options) {
      options$type = env
      engine(options)
    }
  }), envs))
}

#' The N operator
`%n%` = knitr:::`%n%`

#' Processes a block
process_block = function(options) {
  knitr:::eng_block2(options)
}

#' An engine for numbered environments
#' TODO: The engine is activated in knitr by calling it "theorem",
#'       but it is not specific just to theorem, modify this to have a better name
eng_numbered <- function(options) {
  type = options$type %n% 'theorem'
  if (!(type %in% names(theorem_abbr))) stop(
    "The type of theorem '", type, "' is not supported yet."
  )
  options$type = type
  label = paste(theorem_abbr[type], options$label, sep = ':')
  html.before2 = sprintf('(\\#%s) ', label)
  name = options$name
  if (length(name) == 1) {
    options$latex.options = sprintf('[%s]', name)
    html.before2 = paste(html.before2, sprintf('\\iffalse (%s) \\fi{} ', name))
  }
  options$html.before2 = sprintf(
    '<span class="%s" id="%s"><strong>%s</strong></span>', type, label, html.before2
  )
  process_block(options)
}

#' An engine for unumbered environments
#' TODO: The engine is activated in knitr by calling it "proof",
#'       but it is not specific just to theorem, modify this to have a better name
eng_unumbered <- function(options) {
  type = options$type %n% 'proof'
  if (!(type %in% names(label_names_math2))) stop(
    "The type of proof '", type, "' is not supported yet."
  )
  options$type = type
  label = label_prefix(type, label_names_math2)
  name = options$name
  if (length(name) == 1) {
    options$latex.options = sprintf('[%s]', sub('[.]\\s*$', '', name))
    r = '^(.+?)([[:punct:][:space:]]+)$'  # "Remark. " -> "Remark (Name). "
    if (grepl(r, label)) {
      label1 = gsub(r, '\\1', label)
      label2 = paste0(' (', name, ')', gsub(r, '\\2', label))
    } else {
      label1 = label; label2 = ''
    }
    label = sprintf('<em>%s</em>%s', label1, label2)
  } else {
    label = sprintf('<em>%s</em>', label)
  }
  options$html.before2 = sprintf(
    '<span class="%s">%s</span> ', type, label
  )
  options$html.before2 = paste('\\iffalse{}', options$html.before2, '\\fi{}')
  process_block(options)
}
