test_that("list_templates() works", {
  expect_no_error(
    list_templates()
  )
})

test_that("list_templates() returns a data frame", {
  expect_s3_class(
    list_templates(),
    "data.frame"
  )
})
