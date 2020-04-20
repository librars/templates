# Testing render

context("test-render-ymlconfig")

artifacts.index.regular <- file.path("artifacts", "index1.md")

test_that("ordinary index file", {
  yml <- librarstemplates:::generate_config(librarstemplates:::get_file_content(artifacts.index.regular))

  expect_false(is.null(yml))
  expect_false(is.null(yml$rmd_files))
  expect_true(is.vector(yml$rmd_files))
  expect_equal(length(yml$rmd_files), 3 + 1) # index.Rmd added automatically
})
