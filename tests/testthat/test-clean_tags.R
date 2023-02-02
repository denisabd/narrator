test_that("add_tag works", {
  expect_equal(
    clean_tags("<h2> Sales by Region <h2>"),
    "Sales by Region"
  )
})


test_that("add_tag works", {
  expect_equal(
    clean_tags("<h2> Sales by <b>Region<b> <h2>"),
    "Sales by Region"
  )
})

test_that("add_tag throws an error with df supplied", {
  expect_error(
    clean_tags(Sys.Date()),
  )
})


