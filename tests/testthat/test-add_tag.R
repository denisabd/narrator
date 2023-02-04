test_that("add_tag works", {
  expect_equal(
    add_tag("Sales by Region", tag = "h2"),
    "<h2> Sales by Region </h2>"
  )
})

test_that("add_tag works", {
  expect_equal(
    add_tag("Sales by Region", tag = "b"),
    "<b> Sales by Region </b>"
  )
})

test_that("add_tag throws an error with df supplied", {
  expect_error(
    add_tag(sales),
  )
})

test_that("add_tag throws an error with Date supplied", {
  expect_error(
    add_tag(Sys.Date()),
  )
})

