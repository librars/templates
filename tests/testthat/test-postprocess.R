# Testing render

context("test-postprocess")

test <- function(src, expected) {
  src_lines <- librarstemplates:::read_utf8(src)
  expected_lines <- librarstemplates:::read_utf8(expected)

  processed_src_lines <- librarstemplates:::resolve_refs_latex(src_lines)

  expect_equal(length(processed_src_lines), length(expected_lines))

  for (i in 1:length(processed_src_lines)) {
    expect_equal(processed_src_lines[i], expected_lines[i])
  }
}

test_that("resolve labels and references", {
  test(
    file.path("artifacts", "syntax", "labels_refs", "regular.tex"),
    file.path("artifacts", "syntax", "labels_refs", "regular.out.tex")
  )
})
