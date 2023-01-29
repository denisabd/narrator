#' Create Descriptive Narrative
#'
#' @param df Data frame of tibble, can be aggregated or raw
#' @param measure Numeric measure for function to create calculations with,
#' if NULL then it will take the first numeric field available
#' @param dimensions Vector of dimensions for analysis, by default all character
#' or factor variable will be used
#' @param coverage Portion of variability to be covered by narrative, 0 to 1
#' @param coverage_limit Maximum number of elements to be narrated, overrides
#' coverage to avoid extremely verbose narrative creation
#' @param narrative_total glue template for total volumes narrative
#' @param narrative_outlier glue template for single outlier narrative
#' @param narrative_outlier_multiple glue template for multiple outliers narrative
#' @param return_data return a list of variables used in the function's templates
#' @param ... other arguments passed to glue::glue
#'
#' @return character vector, glue
#' @export
#'
#' @examples
#' sales %>%
#' narrate_descriptive(measure = "Sales",
#'             dimensions = c("Territory", "State"))
narrate_descriptive <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    coverage = 0.5,
    coverage_limit = 5,
    narrative_total = "{measure} across all {pluralize(dimension1)} is {total}. ",
    narrative_outlier = "Outlying {dimension} by {measure} is {outlier_insight}. ",
    narrative_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}. ",
    return_data = FALSE,
    ...) {

  if (!is.data.frame(df)) stop("'df' must be a data frame or tibble")

  # Calculating dimensions from a data.frame
  if (is.null(dimensions)) {
    dimensions <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(where(is.character), where(is.factor)) %>%
      names()
  }

  if (length(dimensions) < 1) stop("Desciptive narrative requires at least one dimension")

  # Checking dimensions data types
  dimension_dtypes <- as.character(lapply(df[dimensions], class))

  if (!all(dimension_dtypes %in% c("character", "factor"))) {
    stop(glue::glue("Data types for {toString(dimensions)} must be either 'character' or 'numeric', but is {toString(dimension_dtypes)}"))
  }

  if (is.null(measure)) {
    measures <- df %>%
      dplyr::ungroup() %>%
      dplyr::select_if(is.numeric) %>%
      names()

    if (length(measures) == 0) stop("Desciptive narrative requires a measure")

    measure <- measures[1]
  }

  if (class(df[[measure]]) != "numeric") {
    stop(glue::glue("{measure} must be a numeric column, but is {class(df[[measure]])}"))
  }

  dimension1 <- dimensions[1]

  total <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(!!measure := sum(base::get(measure), na.rm = TRUE)) %>%
    as.matrix() %>%
    as.numeric() %>%
    format_number()

  narrative <- glue::glue(narrative_total, ...)

  variables <- list(narrative_total = narrative,
                    measure = measure,
                    dimension1 <- dimension1,
                    total = total)

  for (dimension in dimensions) {

    table <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(dimension))) %>%
      dplyr::summarise(!!measure := sum(base::get(measure), na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(base::get(measure))) %>%
      dplyr::mutate(share = base::get(measure)/sum(base::get(measure))) %>%
      dplyr::mutate(cum_share = cumsum(share)) %>%
      dplyr::filter(cumsum(lag(cum_share >= coverage, default = FALSE)) == 0)

    n_outliers <- nrow(table)

    outlier_dimension <- table %>%
      dplyr::select(dplyr::all_of(dimension)) %>%
      as.matrix() %>%
      as.character()

    outlier_value <- table %>%
      dplyr::select(dplyr::all_of(measure)) %>%
      as.matrix() %>%
      as.numeric()

    outlier_value_p <- table %>%
      dplyr::mutate(share = round(share * 100, 1)) %>%
      dplyr::select(share) %>%
      as.matrix() %>%
      as.numeric() %>%
      paste("%")

    outlier_insight <- list(outlier_dimension, " (", format_number(outlier_value), ", ", outlier_value_p, ")") %>%
      purrr::pmap(paste0) %>%
      unlist() %>%
      toString()

    if (n_outliers > 1) {
      narrative_outlier_final <- narrative_outlier_multiple
    } else {
      narrative_outlier_final <- narrative_outlier
    }

    variables <- append(variables, list(outlier_insight = outlier_insight))
    variables <- append(variables, list(narrative_outlier_final = glue::glue(narrative_outlier_final, ...)), 1)

    narrative <- glue::glue(narrative,
                            "
                            ",
                            narrative_outlier_final,
                            ...)

    # if (dimension == dimension1 & length(dimensions) > 1) {

    #   df %>%
    #     dplyr::filter()
    # }

  }

  variables <- append(variables, list(narrative = narrative), 1)

  if (return_data == TRUE) {
    return(variables)
  }

  return(narrative)

}
