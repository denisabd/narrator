test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"))
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        date = "Date",
        frequency = "month",
        dimensions = c("Region", "Product"),
        type = 2
      )
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        date = "Date",
        frequency = "month",
        dimensions = c("Region", "Product"),
        type = 3
      )
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        date = "Date",
        frequency = "quarter",
        dimensions = c("Region", "Product"),
        type = 3
      )
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        date = "Date",
        frequency = "week",
        dimensions = c("Region", "Product"),
        type = 2
      )
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      dplyr::mutate(Date = lubridate::floor_date(Date, unit = "month")) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      narrate_trend(type = 1)
  )
})

test_that("narrate_trend() works with average summarization", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        summarization = "average")
  )
})

test_that("narrate_trend() works with count summarization", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Order ID",
        dimensions = c("Region", "Product"),
        summarization = "count")
  )
})

test_that("narrate_trend() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        return_data = TRUE)
  )
})

test_that("narrate_trend() returns list with return_data = TRUE", {
  expect_type(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        return_data = TRUE),
    "list"
  )
})

test_that("narrate_trend() runs with additional arguments", {
  expect_no_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 3,
        use_renviron = TRUE)
  )
})

test_that("narrate_trend() throws an error when coverage is 0", {
  expect_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 0)
  )
})

test_that("narrate_trend() throws an error when coverage_limit is less than 0", {
  expect_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 0)
  )
})

test_that("narrate_trend() throws an error when coverage_limit isn't an integer", {
  expect_error(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 5.5)
  )
})

test_that("narrate_trend() returns a list", {
  expect_type(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product")),
    "list")
})

test_that("narrate_trend() returns a character with simplify = TRUE", {
  expect_type(
    narrator::sales %>%
      narrate_trend(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        simplify = TRUE),
    "character")
})

test_that("narrate_trend() returns an error when no numeric column is available", {
  expect_error(
    narrator::sales %>%
      dplyr::select(Orderdate, Status, Product) %>%
      dplyr::distinct() %>%
      narrate_trend()
  )
})

test_that("narrate_trend() returns an error when df isn't a data.frame", {
  expect_error(
    list(1, 2, 3) %>%
      narrate_trend()
  )
})

test_that("narrate_trend() works when no dimension columns are available", {
  expect_no_error(
    narrator::sales %>%
      dplyr::select(Date, Sales) %>%
      dplyr::distinct() %>%
      narrate_trend()
  )
})
