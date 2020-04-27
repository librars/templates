# add --wrap=preserve to pandoc args for pandoc 2.0:
# https://github.com/rstudio/bookdown/issues/504
pandoc_args2 = function(args) {
  if (pandoc2.0() && !length(grep('--wrap', args))) c('--wrap', 'preserve', args) else args
}

pandoc2.0 = function() rmarkdown::pandoc_available('2.0')