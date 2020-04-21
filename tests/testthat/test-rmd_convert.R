# Testing rmd_convert

context("test-rmd-convert")

artifacts.env.theorem.1 <- file.path("artifacts", "syntax", "env-theorem1.Rmd")
artifacts.env.theorem.1.out <- file.path("artifacts", "syntax", "env-theorem1.out.Rmd")

test <- function(input_file, expected_output_fule) {
  input <- librarstemplates:::normalize_string_newlines(librarstemplates:::get_file_content(input_file))
  expectedOutput <-  librarstemplates:::normalize_string_newlines(librarstemplates:::get_file_content(expected_output_fule))

  output_res <- librarstemplates:::rmdconvert.scan.envs(input)

  expect_false(is.null(output_res$output))
  expect_equal(output_res$output, expectedOutput)
}

test_that("environment", {
  test(artifacts.env.theorem.1, artifacts.env.theorem.1.out)
})
