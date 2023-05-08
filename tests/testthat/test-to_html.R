test_that("to_html() works", {
  narrative <- sales %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, unit = "week")) %>%
    dplyr::group_by(Region, Product, Date) %>%
    dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
    narrate_trend()

  expect_no_error(to_html(narrative))
  expect_s3_class(to_html(narrative), "html")
})

test_that("to_html() works", {
  narrative <- sales %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, unit = "week")) %>%
    dplyr::group_by(Region, Product, Date) %>%
    dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
    narrate_trend(simplify = TRUE)

  expect_no_error(to_html(narrative))
  expect_s3_class(to_html(narrative), "html")
})
