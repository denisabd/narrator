test_that("get_frequency works with monthly data", {
  expect_equal(
    sales %>%
      dplyr::mutate(Date = lubridate::floor_date(Date, unit = "month")) %>%
      get_frequency(),
    "month"
  )
})

test_that("get_frequency works with date field specified", {
  expect_equal(
    sales %>%
      dplyr::mutate(Date_New = lubridate::floor_date(Date, unit = "month")) %>%
      get_frequency(date_field = "Date_New"),
    "month"
  )
})

test_that("get_frequency works with date field specified", {
  expect_error(
    sales %>%
      dplyr::mutate(Date_New = lubridate::floor_date(Date, unit = "month")) %>%
      get_frequency(date_field = "Date_Incorrect")
  )
})

test_that("get_frequency returns error if df isn't a data.frame", {
  expect_error(
    get_frequency(as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01")))
  )
})

test_that("get_frequency works with weekly data", {
  expect_equal(
    sales %>%
      dplyr::mutate(Date = lubridate::floor_date(Date, unit = "week")) %>%
      get_frequency(),
    "week"
  )
})

test_that("get_frequency works with quarterly data", {
  expect_equal(
    sales %>%
      dplyr::mutate(Date = lubridate::floor_date(Date, unit = "quarter")) %>%
      dplyr::group_by(Region, Date) %>%
      dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      get_frequency(),
    "quarter"
  )
})

test_that("get_frequency works with raw data", {
  expect_equal(
    get_frequency(sales),
    "day"
  )
})

test_that("get_frequency returns an error with no date field", {
  expect_equal(
    sales %>%
      dplyr::select(Region, Product) %>%
      get_frequency(),
    NULL
  )
})

test_that("get_frequency returns an error with blank data.frame", {
  expect_error(
    sales %>%
      dplyr::select(Region, Product) %>%
      dplyr::slice(0) %>%
      get_frequency()
  )
})

