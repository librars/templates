# Testing render

context("test-render-ymlconfig")

artifacts.index.regular <- file.path("artifacts", "index1.md")

test_that("ordinary index file", {
  yml <- librarstemplates:::get_configuration(artifacts.index.regular)

  expect_false(is.null(yml))
  expect_false(is.null(yml$rmd_files))
  expect_true(is.vector(yml$rmd_files))
  expect_equal(length(yml$rmd_files), 3)
})
