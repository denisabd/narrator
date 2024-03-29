test_that("format_pct works", {
  expect_equal(
    format_pct("Growth by 24.5 %"),
    "Growth by <b><span style='color: green;'>24.5 %</span></b>"
  )
})

test_that("format_pct works", {
  expect_equal(
    format_pct("-44.5 %"),
    "<b><span style='color: red;'>-44.5 %</span></b>"
  )
})

test_that("format_pct return correct colors", {
  expect_equal(
    unlist(format_pct("Growth by 24.5 %", positive = "steelblue")),
    "Growth by <b><span style='color: steelblue;'>24.5 %</span></b>"
  )
})

test_that("format_pct returns a list", {
  expect_type(
    format_pct(list("Growth by 24.5 %", "-34.2%", "How about 1%")),
    "list"
  )
})

test_that("format_pct returns a character vector", {
  expect_type(
    format_pct(c("Growth by 24.5 %", "-34.2%", "How about 1%")),
    "character"
  )
})

test_that("format_pct returns correct length", {
  expect_length(
    format_pct(c("Growth by 24.5 %", "-34.2%", "How about 1%")),
    3L
  )
})

test_that("format_pct returns an error with non-text or non-list input", {
  expect_error(
    format_pct(sales)
  )
})
