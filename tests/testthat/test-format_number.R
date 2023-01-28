test_that("format_number works", {
  expect_equal(format_number(27334254), "27.3 M")
})

test_that("format_number adds delimeter only for smaller numbers", {
  expect_equal(format_number(273354), "273,354")
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
