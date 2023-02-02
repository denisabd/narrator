test_that("format_num works", {
  expect_equal(format_num(27334254), "27.3 M")
})

test_that("format_num adds delimeter only for smaller numbers", {
  expect_equal(format_num(2734), "2,734")
})

test_that("format_num works with numeric vector", {
  expect_equal(
    format_num(c(28999323, 8573345223, 838473723)),
    c("29 M", "8.6 B", "838.5 M")
  )
})

test_that("format_num works when using lapply() on numeric vector", {
  expect_equal(
    as.character(lapply(c(1824, 234234234), format_num)),
    c("1,824", "234.2 M")
  )
})

test_that("format_num returns character for bigger numbers", {
  expect_type(format_num(27334254), "character")
})

test_that("format_num returns character for smaller numbers", {
  expect_type(format_num(273254), "character")
})

test_that("format_num format override works", {
  expect_type(format_num(2733423254, format = "M"), "character")
})

