#' Create Narrative for Metric Development in Time
#'
#' @inheritParams narrate_descriptive
#' @param date Name of the date column to be used for time based analysis
#' @param frequency Level of time based aggregation for comparing across years 'quarter', 'month', 'week'
#' @param type Type of trend analysis to create: 1 or 'yoy', 2 or 'previous period', 3 or 'same period last year'
#'
#' @return A [list()] of narratives by default and [character()] if `simplify = TRUE`
#' @export
#'
#' @examples
#' sales %>%
#'  dplyr::mutate(Date = lubridate::floor_date(Date, unit = "month")) %>%
#'  dplyr::group_by(Region, Product, Date) %>%
#'  dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
#'  narrate_trend()
#'
#' sales %>%
#'  dplyr::mutate(Date = lubridate::floor_date(Date, unit = "quarter")) %>%
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
    use_chatgpt = FALSE,
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0.5,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0,
    template_total = "From {timeframe_prev} to {timeframe_curr}, {measure} had an {trend} of {change} ({change_p}, {total_prev} to {total_curr}).",
    template_average = "Average {measure} had an {trend} of {change} ({change_p}, {total_prev} to {total_curr}).",
    template_outlier = "{dimension} with biggest changes of {measure} is {outlier_insight}.",
    template_outlier_multiple = "{pluralize(dimension)} with biggest changes of {measure} are {outlier_insight}.",
    template_outlier_l2 = "In {level_l1}, significant {level_l2} by {measure} change is {outlier_insight}.",
    template_outlier_l2_multiple = "In {level_l1}, significant {pluralize(level_l2)} by {measure} change are {outlier_insight}.",
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
  if (coverage_limit%%1!=0) stop("'coverage_limit' must be an integer, no decimals allowed")

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
    stop(glue::glue("{date} must be a date column of class 'Date', 'POSIXct' or 'POSIXlt', but is {class(df[[date]])[1]}"))
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

  # If non-yoy narratives we change df in a way that our ytd/pytd volume
  # functions return required output
  if (type == "previous period") {
    df <- df %>%
      dplyr::filter(base::get(date) %in% c(max_date, prev_date)) %>%
      dplyr::mutate(!!date := ifelse(
        base::get(date) == prev_date,
        max_date - lubridate::years(1),
        base::get(date))
      ) %>%
      dplyr::mutate(!!date := as.Date(base::get(date), origin = "1970-01-01"))

  } else if (type == "same period last year") {
    df <- df %>%
      dplyr::filter(base::get(date) %in% c(max_date, get_py_date(df)))

    # We recalculate prev date for same period of last year to get correct output
    prev_date <- df %>%
      dplyr::filter(base::get(date) != max_date) %>%
      dplyr::select(dplyr::all_of(date)) %>%
      as.matrix() %>%
      as.Date(origin = "1970-01-01") %>%
      max()
  }

  # YoY doesn't compare one period like month or week
  if (type == "yoy") {
    timeframe_prev <- paste(lubridate::year(max_date) - 1, "YTD")
    timeframe_curr <- paste(lubridate::year(max_date), "YTD")
  } else {
    timeframe_curr <- switch(
      frequency,
      "year" = lubridate::year(max_date),
      "quarter" = paste0("Q", lubridate::quarter(max_date), " ", lubridate::year(max_date)),
      "month" = paste(lubridate::month(max_date,label = TRUE, abbr = TRUE), lubridate::year(max_date)),
      "week" = paste("Week", lubridate::week(max_date), lubridate::year(max_date)),
      "day" = paste("Day", lubridate::yday(max_date), lubridate::year(max_date))
    )

    timeframe_prev <- switch(
      frequency,
      "year" = lubridate::year(prev_date),
      "quarter" = paste0("Q", lubridate::quarter(prev_date), " ", lubridate::year(prev_date)),
      "month" = paste(lubridate::month(prev_date,label = TRUE, abbr = TRUE), lubridate::year(prev_date)),
      "week" = paste("Week", lubridate::week(prev_date), lubridate::year(prev_date)),
      "day" = paste("Day", lubridate::yday(prev_date), lubridate::year(prev_date))
    )
  }

  total_curr_raw <- df %>%
    ytd_volume(measure = measure, date = date, summarization = summarization) %>%
    round(1)

  total_prev_raw <- df %>%
    pytd_volume(measure = measure, date = date, summarization = summarization) %>%
    round(1)

  narrative_name <- glue::glue("{timeframe_curr} vs {timeframe_prev}")

  change <- round(total_curr_raw - total_prev_raw, 1)

  if (!is.na(change)) {

    change_p <- paste(round((total_curr_raw/total_prev_raw - 1)*100, 1), "%")
    trend <- ifelse(change > 0, "increase", "decrease")

    if (format_numbers == TRUE) {
      total_curr <- format_num(total_curr_raw, decimals = 2)
      total_prev <- format_num(total_prev_raw, decimals = 2)
      change <- format_num(change, decimals = 2)
    } else {
      total_curr <- total_curr_raw
      total_prev <- total_prev_raw
      change <- format_num(change, decimals = 2)
    }

    # Sum/Count ---------------------------------------------------------------
    if (summarization %in% c("sum", "count")) {

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
          total_curr_raw = total_curr_raw,
          total_prev = total_prev,
          total_prev_raw = total_prev_raw,
          timeframe_curr = timeframe_curr,
          timeframe_prev = timeframe_prev,
          change = change,
          change_p = change_p,
          trend = trend
        )
      ) %>%
        rlang::set_names(narrative_name)

      # Average ------------------------------------------------------------
    } else if (summarization == "average") {

      narrative_average <- glue::glue(
        template_average,
        .transformer = collapse_transformer(sep = collapse_sep, last = collapse_last)
      )

      narrative <- list(narrative_average) %>%
        rlang::set_names(narrative_name)

      variables <- list(
        list(
          narrative_average = narrative_average,
          template_average = template_average,
          measure = measure,
          total_curr = total_curr,
          total_curr_raw = total_curr_raw,
          total_prev = total_prev,
          total_prev_raw = total_prev_raw,
          timeframe_curr = timeframe_curr,
          timeframe_prev = timeframe_prev,
          change = change,
          change_p = change_p,
          trend = trend
        )
      ) %>%
        rlang::set_names(narrative_name)
    }
  } else {
    narrative <- list()
    variables <- list()
  }

  # Adding Date as dimension and leaving last two years only
  if (frequency %in% c("quarter", "month", "week")) {
    time_dimension <- stringr::str_to_title(frequency)

    # Fix if the name of date field is similar to frequency
    if (date == time_dimension) {
      df <- df %>%
        dplyr::rename(Date := !!date)

      date <- "Date"
    }

    df <- df %>%
      dplyr::filter(lubridate::year(base::get(date)) >= lubridate::year(max_date) - 1) %>%
      dplyr::mutate(
        !!time_dimension := switch(
          frequency,
          "quarter" = paste0("Q", lubridate::quarter(base::get(date))),
          "month" = lubridate::month(base::get(date), label = TRUE),
          "week" = paste("Week", lubridate::week(base::get(date))),
        )
      ) %>%
      dplyr::mutate(
        !!time_dimension := as.character(base::get(time_dimension))
      )

    dimensions <- c(dimensions, time_dimension)
  }

  # High-Level Narrative ----------------------------------------------------
  if (length(dimensions) > 0) {

    for (dimension in dimensions) {
      output <- df %>%
        get_trend_outliers(
          dimension = dimension,
          measure = measure,
          # we need overall total for average only, in other cases it leads to incorrect output
          total = switch(summarization,
                         "average" = total_curr_raw - total_prev_raw,
                         "sum" = NULL,
                         "count" = NULL),
          summarization = summarization,
          coverage = coverage,
          coverage_limit = coverage_limit,
          frequency = frequency)

      if (is.null(output)) next

      # Outputting all to the global env
      n_outliers <- output$n_outliers
      outlier_levels <- output$outlier_levels
      outlier_curr_volume <- output$outlier_curr_volume
      outlier_prev_volume <- output$outlier_prev_volume
      trend <- output$trend
      outlier_change <- output$outlier_change
      outlier_change_p <- output$outlier_change_p

      if (format_numbers == TRUE) {
        outlier_curr_volume <- format_num(outlier_curr_volume)
        outlier_prev_volume <- format_num(outlier_prev_volume)
        outlier_change <- format_num(outlier_change)
      }

      ###
      # inc <- which(trend == "increase")
      # dec <- which(trend == "decrease")
      #
      # outlier_insight_inc <- list(
      #   outlier_levels[inc], " (", outlier_change[inc], ", ", outlier_change_p[inc], ", ",
      #   outlier_prev_volume[inc], " to ", outlier_curr_volume[inc],")"
      # ) %>%
      #   purrr::pmap(paste0) %>%
      #   unlist() %>%
      #   toString()
      #
      # outlier_insight_dec <- list(
      #   outlier_levels[dec], " (", outlier_change[dec], ", ", outlier_change_p[dec], ", ",
      #   outlier_prev_volume[dec], " to ", outlier_curr_volume[dec],")"
      # ) %>%
      #   purrr::pmap(paste0) %>%
      #   unlist() %>%
      #   toString()
      ###

      outlier_insight <- list(
        outlier_levels, " (", outlier_change, ", ", outlier_change_p, ", ",
        outlier_prev_volume, " to ", outlier_curr_volume,")"
      ) %>%
        purrr::pmap(paste0)

      if (n_outliers > 1) {
        template_outlier_final <- template_outlier_multiple
        template_selected <- "multiple"
      } else {
        template_outlier_final <- template_outlier
        template_selected <- "single"
      }

      narrative_outlier_final <- glue::glue(
        template_outlier_final,
        .transformer = collapse_transformer(sep = collapse_sep, last = collapse_last)
      )

      outlier_insight <- toString(unlist(outlier_insight))

      if (template_selected == "multiple") {
        variables_l1 <- list(
          list(
            narrative_outlier_final = narrative_outlier_final,
            template_outlier_multiple = template_outlier_multiple,
            dimension = dimension,
            measure = measure,
            outlier_insight = outlier_insight,
            n_outliers = n_outliers,
            outlier_levels = outlier_levels,
            outlier_curr_volume = outlier_curr_volume,
            outlier_prev_volume = outlier_prev_volume,
            outlier_change = outlier_change,
            outlier_change_p = outlier_change_p
          )
        ) %>%
          rlang::set_names(glue::glue("{dimension} by {measure}"))
      } else {
        variables_l1 <- list(
          list(
            narrative_outlier_final = narrative_outlier_final,
            template_outlier = template_outlier,
            dimension = dimension,
            measure = measure,
            outlier_insight = outlier_insight,
            n_outliers = n_outliers,
            outlier_levels = outlier_levels,
            outlier_curr_volume = outlier_curr_volume,
            outlier_prev_volume = outlier_prev_volume,
            outlier_change = outlier_change,
            outlier_change_p = outlier_change_p
          )
        ) %>%
          rlang::set_names(glue::glue("{measure} change by {dimension}"))
      }

      variables <- append(variables, variables_l1)

      narrative <- list(narrative_outlier_final) %>%
        rlang::set_names(glue::glue("{measure} change by {dimension}")) %>%
        append(narrative, after = 0)

      # Detailed Narrative ------------------------------------------------------
      # Getting one level deeper into the outlying dimension
      if (narration_depth > 1 && length(dimensions) > 1
          && match(dimension, dimensions) < length(dimensions)) {

        levels_l1 <- outlier_levels

        for (i in seq_along(levels_l1)) {

          level_l1 <- levels_l1[i]
          level_l2 <- dimensions[which(dimensions == dimension) + 1]

          output <- df %>%
            dplyr::filter(base::get(dimension) %in% levels_l1[i]) %>%
            dplyr::select(-dplyr::all_of(dimension)) %>%
            get_trend_outliers(
              dimension = level_l2,
              measure = measure,
              # we need overall total for average only, in other cases it leads to incorrect output
              total = switch(summarization,
                             "average" = total_curr_raw - total_prev_raw,
                             "sum" = NULL,
                             "count" = NULL),
              summarization = summarization,
              coverage = coverage,
              coverage_limit = coverage_limit,
              frequency = frequency)

          if (is.null(output)) next

          # Outputting all to the global env
          n_outliers <- output$n_outliers
          outlier_levels <- output$outlier_levels
          outlier_curr_volume <- output$outlier_curr_volume
          outlier_prev_volume <- output$outlier_prev_volume
          trend <- output$trend
          outlier_change <- output$outlier_change
          outlier_change_p <- output$outlier_change_p

          if (format_numbers == TRUE) {
            outlier_curr_volume <- format_num(outlier_curr_volume)
            outlier_prev_volume <- format_num(outlier_prev_volume)
            outlier_change <- format_num(outlier_change)
          }

          outlier_insight <- list(
            outlier_levels, " (", outlier_change, ", ", outlier_change_p, ", ",
            outlier_prev_volume, " to ", outlier_curr_volume,")"
          ) %>%
            purrr::pmap(paste0)

          if (n_outliers > 1) {
            template_outlier_l2_final <- template_outlier_l2_multiple
            template_selected <- "multiple"
          } else {
            template_outlier_l2_final <- template_outlier_l2
            tempate_selected <- "single"
          }

          narrative_outlier_l2 <- glue::glue(
            template_outlier_l2_final,
            .transformer = collapse_transformer(sep = collapse_sep, last = collapse_last)
          )

          outlier_insight <- toString(unlist(outlier_insight))

          if (template_selected == "multiple") {
            variables_l2 <- list(
              list(
                narrative_outlier_l2_final = narrative_outlier_l2,
                template_outlier_l2_multiple = template_outlier_l2_multiple,
                level_l1 = level_l1,
                level_l2 = level_l2,
                measure = measure,
                outlier_insight = outlier_insight,
                n_outliers = n_outliers,
                outlier_levels = outlier_levels,
                outlier_curr_volume = outlier_curr_volume,
                outlier_prev_volume = outlier_prev_volume,
                outlier_change = outlier_change,
                outlier_change_p = outlier_change_p
              )
            ) %>%
              rlang::set_names(level_l1)
          } else {
            variables_l2 <- list(
              list(
                narrative_outlier_l2_final = narrative_outlier_l2,
                template_outlier_l2 = template_outlier_l2,
                level_l1 = level_l1,
                level_l2 = level_l2,
                measure = measure,
                outlier_insight = outlier_insight,
                n_outliers = n_outliers,
                outlier_levels = outlier_levels,
                outlier_curr_volume = outlier_curr_volume,
                outlier_prev_volume = outlier_prev_volume,
                outlier_change = outlier_change,
                outlier_change_p = outlier_change_p
              )
            ) %>%
              rlang::set_names(level_l1)
          }


          variables <- append(variables, variables_l2)

          narrative <- list(narrative_outlier_l2) %>%
            rlang::set_names(glue::glue("{level_l1} by {level_l2}")) %>%
            append(narrative, after = 0)
        }
      }
    }
  }

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
  if (length(narrative) == 0) return(NULL)

  if (return_data == TRUE) {
    return(variables)
  }

  if (simplify == TRUE) {
    narrative <- as.character(narrative)
    variables$narrative <- as.character(variables$narrative)
  }

  return(narrative)
}

