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
#' @param template_total glue template for total volumes narrative
#' @param template_outlier glue template for single outlier narrative
#' @param template_outlier_multiple glue template for multiple outliers narrative
#' @param use_renviron Whether to use .Renviron variables in the template
#' @param return_data return a list of variables used in the function's templates
#' @param ... other arguments passed to glue::glue
#'
#' @importFrom rlang :=
#' @importFrom tidyselect where
#' @importFrom stats lag
#'
#' @return character vector, glue
#' @export
#'
#' @examples
#' narrator::sales %>%
#' narrate_descriptive(measure = "Sales",
#'             dimensions = c("Territory", "Product"))
#'
#' narrator::sales %>%
#'   dplyr::filter(Product %in% c("Product A", "Product B")) %>%
#'   dplyr::group_by(Product, Territory)  %>%
#'   dplyr::summarise(Quantity = sum(Quantity)) %>%
#'   narrate_descriptive()
narrate_descriptive <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    coverage = 0.5,
    coverage_limit = 5,
    template_total = "Total {measure} across all {pluralize(dimension1)} is {total}. ",
    template_outlier = "Outlying {dimension} by {measure} is {outlier_insight}. ",
    template_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}. ",
    use_renviron = FALSE,
    return_data = FALSE,
    ...) {

  if (!is.data.frame(df)) stop("'df' must be a data frame or tibble")

  if (coverage_limit < 1) stop("'coverage_limit' must be higher or equal to 1")
  if (coverage_limit%%1!=0) stop("'coverage_limit' must be an interger, no decimals allowed")

  if (coverage <= 0 | coverage > 1) stop("'coverage' must be more than 0 and less or equal to 1")

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
      dplyr::select(where(is.numeric), where(is.integer)) %>%
      names()

    if (length(measures) == 0) stop("Desciptive narrative requires a measure")

    measure <- measures[1]
  }

  if (!class(df[[measure]]) %in% c("numeric", "integer")) {
    stop(glue::glue("{measure} must be a numeric column, but is {class(df[[measure]])}"))
  }

  # Getting Environment Variables if available
  # Candidate for a helper function
  if (use_renviron == TRUE) {
    if (Sys.getenv("descriptive_template_total") != "") {
      template_total <- Sys.getenv("descriptive_template_total")
    }

    if (Sys.getenv("descriptive_template_outlier") != "") {
      template_outlier = Sys.getenv("descriptive_template_outlier")
    }

    if (Sys.getenv("descriptive_template_outlier_multiple") != "") {
      template_outlier_multiple = Sys.getenv("descriptive_template_outlier_multiple")
    }
  }

  dimension1 <- dimensions[1]

  total <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(!!measure := sum(base::get(measure), na.rm = TRUE)) %>%
    as.matrix() %>%
    as.numeric() %>%
    format_number()

  narrative <- glue::glue(template_total, ...)

  variables <- list(template_total = narrative,
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
      dplyr::filter(cumsum(lag(cum_share >= coverage, default = FALSE)) == 0) %>%
      dplyr::slice(1:coverage_limit)

    # For a single dimension we skip to the next level
    if (nrow(table) == 1 & table$cum_share[1] == 1) next

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
      template_outlier_final <- template_outlier_multiple
    } else {
      template_outlier_final <- template_outlier
    }

    variables <- append(variables, list(outlier_insight = outlier_insight))
    variables <- append(variables, list(template_outlier_final = glue::glue(template_outlier_final, ...)), 1)

    narrative <- glue::glue(narrative,
                            "
                            ",
                            template_outlier_final,
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
