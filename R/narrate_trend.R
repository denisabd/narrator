#' Create Narrative for Metric Development in Time
#'
#' @inheritParams narrate_descriptive
#' @param date Name of the date column to be used for time based analysis
#' @param frequency Level of time based aggregation for comparing across years
#' @param type Type of trend analysis to create: 1 or 'yoy', 2 or 'previous period', 3 or 'same period last year'
#'
#' @return A [list()] of narratives by default and [character()] if `simplify = TRUE`
#' @export
#'
#' @examples
#' sales %>%
#'  dplyr::mutate(Date = lubridate::floor_date(Date, unit = "week")) %>%
#'  dplyr::group_by(Region, Product, Date) %>%
#'  dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
#'  narrate_trend()
narrate_trend <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    date = NULL,
    frequency = NULL,
    summarization = "sum",
    type = "yoy",
    coverage = 0.5,
    coverage_limit = 5,
    narration_depth = 2,
    template_total = "From {timeframe_prev} to {timeframe_curr}, {measure} {trend} by {change_ytd}",
    template_average = "Average {measure} across all {pluralize(dimension_one)} is {total}.",
    template_outlier = "Outlying {dimension} by {measure} is {outlier_insight}.",
    template_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}.",
    template_outlier_l2 = "In {level_l1}, significant {level_l2} by {measure} is {outlier_insight}.",
    template_outlier_l2_multiple = "In {level_l1}, significant {pluralize(level_l2)} by {measure} are {outlier_insight}.",
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

  if (coverage_limit < 1) stop("'coverage_limit' must be higher or equal to 1")
  if (coverage_limit%%1!=0) stop("'coverage_limit' must be an interger, no decimals allowed")

  if (coverage <= 0 || coverage > 1) stop("'coverage' must be more than 0 and less or equal to 1")

  # Calculating dimensions from a data.frame
  if (is.null(dimensions)) {
    dimensions <- df %>%
      dplyr::select(where(is.character), where(is.factor)) %>%
      names()
  } else {
    if (!all(dimensions %in% names(df))) {
      stop("all dimensions must be columns the data frame (df)")
    }
  }

  if (length(dimensions) < 1) stop("Trend narrative requires at least one dimension")

  # Checking dimensions data types
  dimension_dtypes <- df %>%
    dplyr::select(dplyr::all_of(dimensions)) %>%
    head() %>%
    dplyr::collect() %>%
    lapply(class)

  if (!all(dimension_dtypes %in% c("character", "factor"))) {
    stop(glue::glue("Data types for {toString(dimensions)} must be either 'character' or 'numeric', but is {toString(dimension_dtypes)}"))
  }

  if (is.null(measure)) {
    measures <- df %>%
      dplyr::select(where(is.numeric), where(is.integer)) %>%
      names()

    if (length(measures) == 0) stop("Trend narrative requires a measure")

    measure <- measures[1]
  }

  if (!class(df[[measure]]) %in% c("numeric", "integer", "character", "factor")) {
    stop(glue::glue("{measure} must be a numeric column, but is {class(df[[measure]])}"))
  }

  if (is.null(date)) {
    date <- df %>%
      dplyr::select_if(lubridate::is.timepoint) %>%
      names() %>%
      magrittr::extract2(1)
  }

  if (length(date) == 0) stop("Trend narrative requires a measure")

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
    if (Sys.getenv("trend_template_total") != "") {
      template_total <- Sys.getenv("trend_template_total")
    }

    if (Sys.getenv("trend_template_average") != "") {
      template_average <- Sys.getenv("trend_template_average")
    }

    if (Sys.getenv("trend_template_outlier") != "") {
      template_outlier = Sys.getenv("trend_template_outlier")
    }

    if (Sys.getenv("trend_template_outlier_multiple") != "") {
      template_outlier_multiple = Sys.getenv("trend_template_outlier_multiple")
    }

    if (Sys.getenv("trend_template_outlier_l2") != "") {
      template_outlier_l2 = Sys.getenv("trend_template_outlier_l2")
    }

    if (Sys.getenv("trend_template_outlier_multiple_l2") != "") {
      template_outlier_multiple_l2 = Sys.getenv("trend_template_outlier_multiple_l2")
    }
  }

  # Total -------------------------------------------------------------------
  # Getting type translation from numbers to actual values
  if (type == 1) {
    type <- "yoy"
  } else if (type == 2) {
    type <- "previous period"
  } else if(type == 3) {
    type <- "same period last year"
  }

  # calculate date features for all narrative types
  max_date <- max(df[[date]])

  prev_date <- df %>%
    dplyr::filter(base::get(date) != max_date) %>%
    dplyr::select(dplyr::all_of(date)) %>%
    as.matrix() %>%
    as.Date(origin = "1970-01-01") %>%
    max()

  if (type == "yoy") {

    timeframe_prev <- paste(lubridate::year(max_date) - 1, "YTD")
    timeframe_curr <- paste(lubridate::year(max_date), "YTD")

    total_curr <- df %>%
      ytd_volume(measure = measure, date = date, summarization = summarization) %>%
      round(1)

    total_prev <- df %>%
      pytd_volume(measure = measure, date = date, summarization = summarization) %>%
      round(1)

    narrative_name <- glue::glue("{timeframe_curr} vs {timeframe_prev}")

  } else if ("previous period") {
    df <- df %>%
      dplyr::filter(base::get(date) %in% c(max_date, prev_date)) %>%
      dplyr::mutate(!!date := ifelse(
        base::get(date) == prev_date,
        max_date - lubridate::years(1),
        base::get(date))
      ) %>%
      dplyr::mutate(!!date := as.Date(base::get(date), origin = "1970-01-01"))

    total_curr <- df %>%
      ytd_volume(measure = measure, date = date, summarization = summarization)

    total_prev <- df %>%
      pytd_volume(measure = measure, date = date, summarization = summarization)

    narrative_name <- glue::glue("{timeframe_curr} vs {timeframe_prev}")

  } else if ("same period last year") {
    df <- df %>%
      dplyr::filter(base::get(date) %in% c(max_date, get_py_date(df)))

    narrative_name <- glue::glue("{timeframe_curr} vs {timeframe_prev}")

  }

  change <- round(total_curr - total_prev, 1)
  change_p <- paste(round((total_curr/total_prev - 1)*100, 1), "%")
  trend <- ifelse(change > 0, "increased", "decreased")

  if (format_numbers == TRUE) {
    total_curr <- format_num(total_curr, decimals = 1)
    total_prev <- format_num(total_prev, decimals = 1)
  }

  template_total <- "From {timeframe_prev} to {timeframe_curr}, {measure} {trend} by {change}"

  narrative_total <- glue::glue(
    template_total,
    .transformer = collapse_transformer(sep = collapse_sep, last = collapse_last)
  )

  narrative <- list(narrative_total) %>%
    rlang::set_names(narrative_name)

  variables <- list(
    list(
      narrative_total = narrative_total,
      template_total = template_total,
      measure = measure,
      total_curr = total_curr,
      total_prev = total_prev,
      timeframe_curr = timeframe_curr,
      timeframe_prev = timeframe_prev,
      change = change,
      change_p = change_p,
      trend = trend
    )
  ) %>%
    rlang::set_names(narrative_name)


  # High-Level Narartive ----------------------------------------------------

  # df %>%
  #   dplyr::group_by(dplyr::across(dplyr::all_of(dimensions))) %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(
  #     ytd_volume = purrr::map_dbl(data, ytd_volume),
  #     pytd_volume = purrr::map_dbl(data, pytd_volume)
  #   )

  # dimension_one <- dimensions[1]


  # Detailed Narrative ------------------------------------------------------


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

#' Get prior year max date from a data frame or time series. This is especially useful for weekly time series when YTD PYTD calculations aren't as straightforward.
#' For example if "2022-05-23" is max date in weekly aggregates time series data frame is doesn't meant that PYTD will end exactly a year ago - we need to calculate number of weeks passed to get the calculation right
#'
#' @param df Data frame or tibble with date column
#' @param frequency Date frequency
#'
#' @return \code{Date} object
#' @noRd
get_py_date <- function(df, frequency = NULL) {

  df <- df %>%
    dplyr::ungroup()

  if (is.null(frequency)) {
    frequency <- get_frequency(df)
  }

  date_field <- df %>%
    dplyr::select_if(lubridate::is.timepoint) %>%
    names() %>%
    magrittr::extract2(1)

  # rename to avoid ambiguous objects to week - name of the column and function
  if (date_field == "week") {
    df <- df %>%
      dplyr::rename(date_column = week)

    date_field <- "date_column"
  }

  if (length(date_field) == 0) stop("data frame must contain one date column")

  max_date <- max(df[[date_field]])
  max_year <- lubridate::year(max_date)

  if (frequency == "week") {
    df_weekly <- df %>%
      dplyr::select(dplyr::all_of(date_field)) %>%
      unique() %>%
      dplyr::arrange(base::get(date_field)) %>%
      dplyr::mutate(
        week = lubridate::week(base::get(date_field)),
        year = lubridate::year(base::get(date_field))
      )

    max_week <- df_weekly %>%
      dplyr::filter(base::get(date_field) == max_date) %>%
      dplyr::select(week) %>%
      as.numeric()

    py_date <- df_weekly %>%
      dplyr::filter(year == max_year-1,
                    week == max_week) %>%
      dplyr::select(dplyr::all_of(date_field)) %>%
      as.matrix() %>%
      as.Date()

    if (length(py_date) == 0) py_date <- max_date - lubridate::years(1)

  } else {
    py_date <- max_date - lubridate::years(1)
  }

  return(py_date)
}


#' Calculate YTD volume from raw dataset
#'
#' @param df data frame or tibble to be filtered (raw, not generated by gen_table)
#' @param measure variable to be analyzed
#' @param date list of columns to apply filter
#' @param summarization summarization field in series, one of the following methods - 'count', 'average', 'sum'
#' @param current_year add current year to avoid situation when some of the groups don't have data in latest periods and current year is estimated incorrectly
#' @param cy_date date cutoff for current year if different from the max date of the supplied data frame
#'
#' @return \code{numeric} value
#' @noRd
ytd_volume <- function(
    df,
    measure = NULL,
    date = NULL,
    summarization = "sum",
    current_year = NULL,
    cy_date = NULL) {


  # Table must be a data.frame and have at least one row
  if (!is.data.frame(df)) stop("df must be a data.frame or tibble")
  if (nrow(df) == 0) stop("df must have at least one row, execution is stopped")

  # Getting the right data types
  df <- df %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    readr::type_convert(na = c("")) %>%
    suppressMessages() %>%
    suppressWarnings()

  # Summarization Assertion
  if (!summarization %in% c("sum", "count", "average")) stop("summarization must of be one of: 'sum', 'count' or 'mean'.")

  # Measure, Date and Dimensions Assertion
  if (!is.null(measure)) {
    if (!measure %in% names(df)) stop("measure must a column in the dataset")
  } else {
    # If measure isn't supplied get the first numerical column from it
    measure <- df %>%
      dplyr::select_if(is.numeric) %>%
      names() %>%
      magrittr::extract2(1)
  }

  # Get Date
  if (!is.null(date)) {
    if (!date %in% names(df)) stop("date must a column in the dataset")

    df <- df %>%
      dplyr::mutate(!!date := as.Date(base::get(date)))

    if (!lubridate::is.timepoint(df[[date]])) stop("'date' must be a date column in the dateset")
  } else {
    # Getting the first date field available
    date <- df %>%
      dplyr::select_if(lubridate::is.timepoint) %>%
      names() %>%
      magrittr::extract2(1) # Get the first date field available
  }

  # Current Year's Date
  if (is.null(cy_date)) {
    cy_date <- max(df[[date]])
  } else {
    cy_date <- as.Date(cy_date)
  }

  # Current year assertion
  if (!is.null(current_year) && current_year != lubridate::year(max(df[[date]]))) {
    current_year <- suppressWarnings(as.numeric(current_year))
    if (is.na(current_year)) stop("current_year argument must be numeric or convertable to numeric like 2022 or '2022' ")
  } else {
    current_year <- lubridate::year(cy_date)
  }

  cy_volume <- df %>%
    dplyr::mutate(year = lubridate::year(base::get(date))) %>%
    dplyr::filter(year == current_year,
                  base::get(date) <= cy_date) %>%
    dplyr::summarise(value = switch(
      summarization,
      "sum" = sum(base::get(measure),na.rm = TRUE),
      "count" = length(unique(base::get(measure))),
      "average" = mean(base::get(measure),na.rm = TRUE)
    )
    )  %>%
    as.numeric()

  return(cy_volume)
}

#' Calculate PYTD volume from raw dataset
#'
#' @param df data frame or tibble to be filtered (raw, not generated by gen_table)
#' @param measure variable to be analyzed
#' @param date list of columns to apply filter
#' @param summarization summarization field in series, one of the following methods - 'count', 'mean', 'sum'
#' @param current_year add current year to avoid situation when some of the groups don't have data in latest periods and current year is estimated incorrectly
#' @param py_date date to be used for PYTD calculation, if NULL then \code{get_py_date()} will be used by default
#'
#' @return \code{numeric} value
#' @noRd
pytd_volume <- function(
    df,
    measure = NULL,
    date = NULL,
    summarization = "sum",
    current_year = NULL,
    py_date = NULL) {

  # Table must be a data.frame and have at least one row
  if (!is.data.frame(df)) stop("df must be a data.frame or tibble")
  if (nrow(df) == 0) stop("df must have at least one row, execution is stopped")

  # Getting the right data types
  df <- df %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    readr::type_convert(na = c("")) %>%
    suppressMessages() %>%
    suppressWarnings()

  # Summarization Assertion
  if (!summarization %in% c("sum", "count", "average")) stop("summarization must of be one of: 'sum', 'count' or 'mean'.")

  # Measure, Date and Dimensions Assertion
  if (!is.null(measure)) {
    if (!measure %in% names(df)) stop("measure must a column in the dataset")
  } else {
    # If measure isn't supplied get the first numerical column from it
    measure <- df %>%
      dplyr::select_if(is.numeric) %>%
      names() %>%
      magrittr::extract2(1)
  }

  # Get Date
  if (!is.null(date)) {
    if (!date %in% names(df)) stop("date must a column in the dataset")

    df <- df %>%
      dplyr::mutate(!!date := as.Date(base::get(date)))

    if (!lubridate::is.timepoint(df[[date]])) stop("'date' must be a date column in the dateset")
  } else {
    # Getting the first date field available
    date <- df %>%
      dplyr::select_if(lubridate::is.timepoint) %>%
      names() %>%
      magrittr::extract2(1) # Get the first date field available
  }

  # PY Date
  if (is.null(py_date)) {
    py_date <- get_py_date(df)
  } else {
    py_date <- as.Date(py_date)
  }

  # Current year assertion
  if (!is.null(current_year) && current_year != lubridate::year(max(df[[date]]))) {

    current_year <- suppressWarnings(as.numeric(current_year))

    if (is.na(current_year)) {
      stop("current_year argument must be numeric or convertable to numeric like 2022 or '2022' ")
    }

    # we need py_date since we don't have current year's max date here.
    previous_year <- current_year - 1

    py_date <- as.Date(py_date)
  } else {
    previous_year <- lubridate::year(py_date)
  }

  py_volume <- df %>%
    dplyr::mutate(year = lubridate::year(base::get(date))) %>%
    dplyr::filter(year == previous_year,
                  base::get(date) <= py_date) %>%
    dplyr::summarise(value = switch(
      summarization,
      "sum" = sum(base::get(measure),na.rm = TRUE),
      "average" = mean(base::get(measure),na.rm = TRUE),
      "count" = length(unique(base::get(measure)))
    )
    ) %>%
    as.numeric()

  return(py_volume)
}

