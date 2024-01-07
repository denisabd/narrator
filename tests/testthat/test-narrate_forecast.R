test_that("narrate_forecast() works", {
  testthat::skip_if_not_installed("prophet")

  fit_prophet <- function(data) {
    model <- suppressMessages(prophet::prophet(data))
    future <- prophet::make_future_dataframe(model, periods = 12, freq = "month")
    forecast <- predict(model, future)
    return(forecast)
  }

  grouped_data <- sales %>%
    dplyr::mutate(ds = lubridate::floor_date(Date, unit = "month")) %>%
    dplyr::group_by(Region, ds) %>%
    dplyr::summarise(y = sum(Sales, na.rm = TRUE)) %>%
    tidyr::nest()

  grouped_data$forecast <- lapply(grouped_data$data, fit_prophet)

  actuals <- grouped_data %>%
    dplyr::select(-forecast) %>%
    tidyr::unnest(data)

  df <- grouped_data %>%
    dplyr::select(-data) %>%
    tidyr::unnest(forecast) %>%
    dplyr::select(ds, yhat) %>%
    dplyr::left_join(actuals) %>%
    suppressMessages()

  expect_no_error(narrate_forecast(df, actuals = "y", forecast = "yhat"))
  expect_no_error(narrate_forecast(df, actuals = "y", forecast = "yhat"))
  expect_type(narrate_forecast(df, actuals = "y", forecast = "yhat"), "list")
  expect_type(narrate_forecast(df, actuals = "y", forecast = "yhat"), "list")
  expect_type(narrate_forecast(df, actuals = "y", forecast = "yhat", return_data = TRUE), "list")
  expect_type(narrate_forecast(df, actuals = "y", forecast = "yhat", simplify = TRUE), "character")
  expect_error(narrate_forecast(df, actuals = "Acts", forecast = "Fct"))
  expect_error(narrate_forecast(df, actuals = "y", forecast = "yhat", coverage_limit = 0))
  expect_error(narrate_forecast(df, actuals = "y", forecast = "yhat", coverage_limit = 1.5))

  expect_error({
    df %>%
      dplyr::select(-ds) %>%
      narrate_forecast(actuals = "y", forecast = "yhat")
  })

  expect_error({
    df %>%
      dplyr::select(-y) %>%
      narrate_forecast(actuals = "y", forecast = "yhat")
  })

  expect_error({
    df %>%
      dplyr::select(-yhat) %>%
      narrate_forecast(actuals = "y", forecast = "yhat")
  })
})

