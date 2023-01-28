test_that("pluralize works", {
  expect_equal(pluralize(c("man", "Segment", "mouse", "forecast")), c("men", "Segments", "mice", "forecasts"))
})

test_that("pluralize returns error with numeric", {
  expect_error(pluralize(12))
})
