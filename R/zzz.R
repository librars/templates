#' Executed when loading the package
.onLoad = function(lib, pkg) {
  message("Loading engines")
  register_knitr_eng(names(theorem_abbr), eng_numbered)
  register_knitr_eng(names(label_names_math2), eng_unumbered)
}