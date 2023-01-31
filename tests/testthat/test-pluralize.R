test_that("pluralize works", {
  expect_equal(pluralize(c("man", "Segment", "mouse", "forecast")), c("men", "Segments", "mice", "forecasts"))
})

test_that("pluralize works", {
  expect_equal(
    pluralize("person", n = 5, prepend = TRUE),
    "5 people"
  )
})

test_that("pluralize returns error with numeric", {
  expect_error(pluralize(12))
})

test_that("is_plural() words", {
  expect_equal(
    is_plural("People"),
    TRUE
  )
})

test_that("is_plural() words", {
  expect_equal(
    is_plural("Sales"),
    TRUE
  )
})

test_that("is_plural() words", {
  expect_equal(
    is_singular("function"),
    TRUE
  )
})
