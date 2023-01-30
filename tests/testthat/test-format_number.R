test_that("format_number works", {
  expect_equal(format_number(27334254), "27.3 M")
})

test_that("format_number adds delimeter only for smaller numbers", {
  expect_equal(format_number(273354), "273,354")
})

test_that("format_number works with numeric vector", {
  expect_equal(
    format_number(c(28999323, 8573345223, 838473723)),
    c("29 M", "8.6 B", "838.5 M")
  )
})

test_that("format_number works when using lapply() on numeric vector", {
  expect_equal(
    as.character(lapply(c(13824, 234234234), format_number)),
    c("13,824", "234.2 M")
  )
})

test_that("format_number returs character for bigger numbers", {
  expect_type(format_number(27334254), "character")
})

test_that("format_number returs character for smaller numbers", {
  expect_type(format_number(273254), "character")
})

test_that("format_number format override works", {
  expect_type(format_number(2733423254, format = "M"), "character")
})
