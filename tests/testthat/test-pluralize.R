test_that("pluralize works", {
  expect_equal(
    pluralize(c("man", "Segment", "mouse", "forecast")),
    c("men", "Segments", "mice", "forecasts"))
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

test_that("is_plural() works", {
  expect_equal(
    is_plural("People"),
    TRUE
  )
})

test_that("is_plural() works", {
  expect_equal(
    is_plural("Sales"),
    TRUE
  )
})

test_that("is_plural() works", {
  expect_equal(
    is_singular("function"),
    TRUE
  )
})

test_that("singularize works", {
  expect_equal(
    singularize(c("men", "Segments", "mice", "forecasts")),
    c("man", "Segment", "mouse", "forecast")
  )
})

test_that("singularize returns error with numeric", {
  expect_error(singularize(12))
})
