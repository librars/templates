# default names for labels
label_names <- list(fig = 'Figure ', tab = 'Table ', eq = 'Equation ')

# prefixes for theorem environments
theorem_abbr <- c(
  theorem = 'thm', lemma = 'lem', corollary = 'cor', proposition = 'prp', conjecture = 'cnj',
  definition = 'def', example = 'exm', exercise = 'exr'
)
# numbered math environments
label_names_math <- setNames(list(
  'Theorem ', 'Lemma ', 'Corollary ', 'Proposition ', 'Conjecture ', 'Definition ', 'Example ', 'Exercise '
), theorem_abbr)

# unnumbered math environments
label_names_math2 <- list(proof = 'Proof. ', remark = 'Remark. ', solution = 'Solution. ')

all_math_env <- c(names(theorem_abbr), names(label_names_math2))

label_names <- c(label_names, label_names_math)

# types of labels currently supported, e.g. \(#fig:foo), \(#tab:bar)
label_types = names(label_names)
reg_label_types = paste(label_types, collapse = '|')

# given a label, e.g. fig:foo, figure out the appropriate prefix
label_prefix <- function(type, dict = label_names) {
  i18n('label', type, dict)
}

i18n = function(group, key, dict = list()) {
  #labels = load_config()[['language']][[group]]
  labels = NULL # TODO
  if (is.null(labels[[key]])) dict[[key]] else labels[[key]]
}
