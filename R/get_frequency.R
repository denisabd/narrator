#' Get Date Frequency of the Data
#'
#' @description Function will group the data and estimate the frequency of the time stamps,
#' returning 'year', 'quarter', 'month', 'week' or 'day'. You can use it on raw or aggregated data frames
#'
#' @param df [data.frame()] or [tibble()] Data frame of tibble, can be aggregated or raw
#' @param date_field Date field to be analyzed, by default the first date-like column will be used
#'
#' @return frequency - "quarter", "month", "week" or "day"
#' @export
#'
#' @examples
#' sales %>%
#'   dplyr::mutate(Date = lubridate::floor_date(Date, unit = "month")) %>%
#'   get_frequency()
#'
#' sales %>%
#'   dplyr::mutate(Date = lubridate::floor_date(Date, unit = "week")) %>%
#'   get_frequency()
#'
#' sales %>%
#'   dplyr::mutate(Date = lubridate::floor_date(Date, unit = "quarter")) %>%
#'   dplyr::group_by(Region, Date) %>%
#'   dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
#'   get_frequency()
#'
#' get_frequency(sales)
get_frequency <- function(df, date_field = NULL) {

  if (!is.data.frame(df)) {
    stop("'df' must be a data.frame")
  }

  if (nrow(df) == 0) stop("'df' must have at least 1 row")

  df <- df %>%
    dplyr::ungroup()

  if (is.null(date_field)) {
    date_field <- df %>%
      dplyr::select_if(lubridate::is.timepoint) %>%
      names()
  } else {
    if (!date_field %in% names(df)) {
      stop("'date_field' must be present in the supplied data frame")
    }
  }

  if (length(date_field) == 0) {
    return(NULL)
  } else {
    date_field <- date_field[1]
  }

  dimensions <- df %>%
    dplyr::select(where(is.character), where(is.factor)) %>%
    names()

  df <- df %>%
    dplyr::rename(date_field = dplyr::all_of(date_field))

  est_frequency <- df %>%
    dplyr::select(date_field) %>%
    dplyr::distinct() %>%
    dplyr::arrange(date_field) %>%
    dplyr::mutate(diff = as.numeric(date_field - dplyr::lag(date_field))) %>%
    dplyr::count(diff) %>%
    dplyr::slice(which.max(n)) %>%
    dplyr::select(diff) %>%
    as.numeric() %>%
    abs()

  frequency <- dplyr::case_when(
    est_frequency > 300 ~ "year",
    est_frequency > 35 ~ "quarter",
    est_frequency > 8 ~ "month",
    est_frequency > 3 ~ "week",
    TRUE ~ "day"
  )

  return(frequency)
}

