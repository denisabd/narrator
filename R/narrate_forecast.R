#' Create Narrative for Time Series Forecast Data Frames
#'
#' @inheritParams narrate_trend
#' @param forecast Name of the forecast column in the data frame
#' @param actuals Name of the actuals column in the data frame
#' @param template_cy \code{\link[glue]{glue}} template for current year volumes narrative
#' @param template_ftm \code{\link[glue]{glue}} template for future 12 months projection
#' @param template_ftm_change \code{\link[glue]{glue}} template for projected change in the next 12 months
#'
#' @return A [list()] of narratives by default and [character()] if `simplify = TRUE`
#' @export
#'
#' @examples
#' library(prophet)
#' library(dplyr)
#' library(tidyr)
#'
#' fit_prophet <- function(data) {
#'   model <- prophet(data)
#'   future <- make_future_dataframe(model, periods = 12, freq = "month")
#'   forecast <- predict(model, future)
#'   return(forecast)
#' }
#' grouped_data <- sales %>%
#'   dplyr::mutate(ds = lubridate::floor_date(Date, unit = "month")) %>%
#'   dplyr::group_by(Region, ds) %>%
#'   dplyr::summarise(y = sum(Sales, na.rm = TRUE)) %>%
#'   tidyr::nest()
#'
#' grouped_data$forecast <- lapply(grouped_data$data, fit_prophet)
#'
#' actuals <- grouped_data %>%
#'   dplyr::select(-forecast) %>%
#'   unnest(data)
#'
#' df <- grouped_data %>%
#'   dplyr::select(-data) %>%
#'   unnest(forecast) %>%
#'   dplyr::select(ds, yhat) %>%
#'   dplyr::left_join(actuals) %>%
#'   dplyr::rename(Actuals = y,
#'                 Forecast = yhat)
#'
#' narrate_forecast(df)
narrate_forecast <- function(
    df,
    date = NULL,
    frequency = NULL,
    summarization = "sum",
    type = "yoy",
    coverage = 0.5,
    coverage_limit = 5,
    narration_depth = 2,
    use_chatgpt = FALSE,
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0.5,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0,
    forecast = "Forecast",
    actuals = "Actuals",
    template_cy = "Forecasted volumes for {current_year} are equal to {format_num(cy_forecast)}",
    template_ftm = "Overall forecast for the next 12 months is {format_num(ftm_forecast)}",
    template_ftm_change = "Projected {trend} in the next 12 months is equal to {format_num(ftm_change)} ({ftm_change_p}%).",
    use_renviron = FALSE,
    return_data = FALSE,
    simplify = FALSE,
    format_numbers = TRUE,
    collapse_sep = ", ",
    collapse_last = " and ",
    ...) {

  # Assertion ---------------------------------------------------------------
  if (!is.data.frame(df) && !dplyr::is.tbl(df)) stop("'df' must be a data frame, tibble, or dplyr::tbl connection")

  df <- df %>%
    dplyr::ungroup()

  if (!actuals %in% names(df)) stop(glue::glue("{actuals} must be a column in the data frame"))
  if (!forecast %in% names(df)) stop(glue::glue("{forecast} must be a column in the data frame"))

  if (coverage_limit < 1) stop("'coverage_limit' must be higher or equal to 1")
  if (coverage_limit%%1!=0) stop("'coverage_limit' must be an interger, no decimals allowed")

  if (coverage <= 0 || coverage > 1) stop("'coverage' must be more than 0 and less or equal to 1")

  # Assertion for actuals and forecast value
  if (!class(df[[actuals]]) %in% c("numeric", "integer", "character", "factor")) {
    stop(glue::glue("{actuals} must be a numeric column, but is {class(df[[actuals]])}"))
  }

  if (!class(df[[forecast]]) %in% c("numeric", "integer", "character", "factor")) {
    stop(glue::glue("{forecast} must be a numeric column, but is {class(df[[forecast]])}"))
  }

  if (is.null(date)) {
    date <- df %>%
      dplyr::select_if(lubridate::is.timepoint) %>%
      names()

    if (length(date) > 0) {
      date <- date[1]
    } else {
      stop("Date column is required in 'Date', 'dttm', 'POSIXlt' or 'POSIXct' formats")
    }
  }

  # leaving only the neccessary fields
  df <- df %>%
    dplyr::select(dplyr::all_of(c(date, forecast, actuals)))

  if (!date %in% names(df)) stop("{date} is not a column in the data frame")

  if (!any(class(df[[date]]) %in% c("Date", "POSIXct", "POSIXlt"))) {
    stop(glue::glue("{date} must be a numeric column, but is {class(df[[date]])[1]}"))
  }

  if (is.null(frequency)) {
    frequency <- narrator::get_frequency(df[date])
  } else {
    df <- df %>%
      dplyr::mutate(!!date := switch(
        frequency,
        "quarter" = lubridate::floor_date(base::get(date), unit = "quarter"),
        "month" = lubridate::floor_date(base::get(date), unit = "month"),
        "week" = lubridate::floor_date(base::get(date), unit = "week", week_start = 1),
        "day" = base::get(date)
      )
      )
  }

  # Renviron ----------------------------------------------------------------
  # Getting Environment Variables if available
  # Candidate for a helper function
  if (!is.null(getOption("narrator.use_renviron"))) {
    use_renviron <- getOption("narrator.use_renviron")
  } else if (Sys.getenv("use_renviron") != "") {
    use_renviron <- Sys.getenv("use_renviron")
  }

  if (use_renviron == TRUE) {
    if (Sys.getenv("forecast_template_cy") != "") {
      template_cy <- Sys.getenv("forecast_template_cy")
    }

    if (Sys.getenv("forecast_template_ftm") != "") {
      template_ftm <- Sys.getenv("forecast_template_ftm")
    }

    if (Sys.getenv("forecast_template_ftm_change") != "") {
      template_ftm <- Sys.getenv("forecast_template_ftm_change")
    }
  }

  # Total -------------------------------------------------------------------
  # Transforming dttm or similar formats to date and avoiding time zone issues
  df <- df %>%
    dplyr::mutate(!!date := as.Date(base::get(date)))

  # Current Year
  max_actuals_date <- df %>%
    dplyr::filter(!is.na(base::get(actuals))) %>%
    dplyr::select(dplyr::all_of(date)) %>%
    as.matrix() %>%
    as.Date() %>%
    max()

  next_date <- switch(
    frequency,
    "week" = max_actuals_date + lubridate::weeks(1),
    "month" = max_actuals_date + months(1),
    "quarter" = max_actuals_date + months(3)
  )

  max_forecast_date <- df %>%
    dplyr::select(dplyr::all_of(date)) %>%
    as.matrix() %>%
    as.Date() %>%
    max()

  current_year <- lubridate::year(max_actuals_date)

  # If current year is complete and nothing was forecast we add different narrative
  if (lubridate::year(next_date) == current_year + 1) {
    template_cy <- "Actuals for {current_year} are equal to {format_num(cy_forecast)}"
  }

  cy_forecast <- df %>%
    dplyr::filter(lubridate::year(base::get(date)) == current_year) %>%
    dplyr::mutate(!!forecast := ifelse(is.na(base::get(actuals)), base::get(forecast), base::get(actuals))) %>%
    dplyr::summarise(!!forecast := sum(base::get(forecast), na.rm = TRUE)) %>%
    as.numeric() %>%
    round(2)

  narrative_cy <- glue::glue(template_cy)

  narrative <- list(narrative_cy) %>%
    rlang::set_names("Current Year Actuals")

  variables <- list(
    list(
      template_cy = narrative_cy,
      current_year = current_year,
      max_actuals_date = max_actuals_date,
      max_forecast_date = max_forecast_date,
      frequency = frequency,
      cy_forecast = cy_forecast
    )
  ) %>%
    rlang::set_names("Current Year Actuals")

  # Next 12 months
  # In case we have at least one year of forecast values narrate FTM
  # We count it as at least 52 weeks
  if (as.numeric(max_forecast_date - max_actuals_date)/7 >= 52 & !is.null(template_ftm)) {
    ftm_forecast <- df %>%
      dplyr::filter(base::get(date) > max_actuals_date,
                    base::get(date) <= max_actuals_date + months(12)) %>%
      dplyr::summarise(!!forecast := sum(base::get(forecast), na.rm = TRUE)) %>%
      as.numeric() %>%
      round(2)

    narrative_ftm <- glue::glue(template_ftm)

    narrative <- list(narrative_ftm) %>%
      rlang::set_names("12 Month Projection") %>%
      append(narrative, after = 0)

    variables_ftm <- list(
      list(
        narrative_ftm = narrative_ftm,
        template_ftm = template_ftm,
        ftm_forecast = ftm_forecast
      )
    ) %>%
      rlang::set_names("12 Month Projection")

    variables <- append(variables, variables_ftm)

    if (!is.null(template_ftm_change)) {
      ltm_actuals <- df %>%
        dplyr::filter(!is.na(base::get(actuals)),
                      base::get(date) > max_actuals_date - months(12)) %>%
        dplyr::summarise(!!forecast := sum(base::get(actuals), na.rm = TRUE)) %>%
        as.numeric() %>%
        round(2)

      trend <- ifelse(ftm_forecast > ltm_actuals, "increase", "decrease")
      ftm_change <- ftm_forecast - ltm_actuals
      ftm_change_p <- round(ftm_change/ltm_actuals*100, 2)

      narrative_ftm_change <- glue::glue(template_ftm_change)

      narrative <- list(narrative_ftm_change) %>%
        rlang::set_names(glue::glue("Overall {trend} the next 12 months")) %>%
        append(narrative, after = 0)

      variables_ftm_change <- list(
        list(
          narrative_ftm_change = narrative_ftm_change,
          template_ftm_change = template_ftm_change,
          trend = trend,
          ltm_actuals = ltm_actuals
        )
      ) %>%
        rlang::set_names(glue::glue("Overall {trend} the next 12 months"))

      variables <- append(variables, variables_ftm_change)
    }
  }

  # Trend -------------------------------------------------------------------
  trend_df <- df %>%
    dplyr::mutate(!!forecast := ifelse(
      !is.na(base::get(actuals)),
      base::get(actuals),
      base::get(forecast)
    )
    ) %>%
    dplyr::select(-dplyr::all_of(actuals))

  trend_narrative <- trend_df %>%
    narrator::narrate_trend(
      type = type,
      coverage = coverage,
      coverage_limit = coverage_limit,
      narration_depth = narration_depth
    )

  variables_trend <- trend_df %>%
    narrator::narrate_trend(
      type = type,
      coverage = coverage,
      coverage_limit = coverage_limit,
      narration_depth = narration_depth,
      return_data = TRUE
    )

  narrative <- append(narrative, trend_narrative)
  variables <- append(variables, variables_trend[names(variables_trend) != "narrative"])

  variables <- append(variables, list(narrative = narrative), 0)

  # ChatGPT -----------------------------------------------------------------
  if (use_chatgpt) {
    narrative <- enhance_narrative(
      narrative,
      openai_api_key = openai_api_key,
      max_tokens = max_tokens,
      temperature = temperature,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      presence_penalty = presence_penalty
    )
  }

  # Output ------------------------------------------------------------------
  if (return_data == TRUE) {
    return(variables)
  }

  if (simplify == TRUE) {
    narrative <- as.character(narrative)
    variables$narrative <- as.character(variables$narrative)
  }

  return(narrative)
}

