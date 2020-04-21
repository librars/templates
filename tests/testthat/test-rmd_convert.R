# Testing rmd_convert

context("test-rmd-convert")

test <- function(input_file, expected_output_fule) {
  input <- librarstemplates:::normalize_string_newlines(librarstemplates:::get_file_content(input_file))
  expectedOutput <-  librarstemplates:::normalize_string_newlines(librarstemplates:::get_file_content(expected_output_fule))

  output_res <- librarstemplates:::rmdconvert.scan.envs(input)

  expect_false(is.null(output_res$output))
  expect_equal(output_res$output, expectedOutput)
}

test_batch <- function(batch) {
  for (artifact_couple in batch) {
    test(artifact_couple$src, artifact_couple$out)
  }
}

### Tests for construct: 'environment' ###

artifacts.env.batch <- list(
  list(
    # Basic environment, space between #! and name, surrounded by text, double empty lines as ending
    src = file.path("artifacts", "syntax", "environment", "env-theorem1.Rmd"),
    out = file.path("artifacts", "syntax", "environment", "env-theorem1.out.Rmd")
  ),
  list(
    # Basic environment, space between #! and name, surrounded by text, new environment as ending
    src = file.path("artifacts", "syntax", "environment", "env-theorem2.Rmd"),
    out = file.path("artifacts", "syntax", "environment", "env-theorem2.out.Rmd")
  )
)

test_that("environment", {
  test_batch(artifacts.env.batch)
})
