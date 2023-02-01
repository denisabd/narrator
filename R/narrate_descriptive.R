#' Create Descriptive Narrative
#'
#' @param df Data frame of tibble, can be aggregated or raw
#' @param measure Numeric measure for function to create calculations with,
#' if NULL then it will take the first numeric field available
#' @param dimensions Vector of dimensions for analysis, by default all character
#' or factor variable will be used
#' @param summarization Approach for data summarization/aggregation - 'sum', 'count' or 'mean'
#' @param coverage Portion of variability to be covered by narrative, 0 to 1
#' @param coverage_limit Maximum number of elements to be narrated, overrides
#' coverage to avoid extremely verbose narrative creation
#' @param narration_depth Parameter to control the depth of the analysis 1 for summary and 2 for detailed
#' @param template_total glue template for total volumes narrative
#' @param template_outlier glue template for single outlier narrative
#' @param template_outlier_multiple glue template for multiple outliers narrative
#' @param template_outlier_l2 glue template for deeper hierarchical single outlier narrative
#' @param template_outlier_l2_multiple glue template for deeper hierarchical multiple outliers narrative
#' @param use_renviron Whether to use .Renviron variables in the template
#' @param return_data return a list of variables used in the function's templates
#' @param simplify if TRUE - returns a character vector, if FALSE - named list
#' @param ... other arguments passed to glue::glue
#'
#' @importFrom rlang :=
#' @importFrom tidyselect where
#'
#' @return character vector, glue
#' @export
#'
#' @examples
#' narrator::sales %>%
#' narrate_descriptive(measure = "Sales",
#'             dimensions = c("Region", "Product"))
#'
#' narrator::sales %>%
#'   dplyr::filter(Product %in% c("Product A", "Product B")) %>%
#'   dplyr::group_by(Product, Region)  %>%
#'   dplyr::summarise(Quantity = sum(Quantity)) %>%
#'   narrate_descriptive()
narrate_descriptive <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    summarization = "sum",
    coverage = 0.5,
    coverage_limit = 5,
    narration_depth = 2,
    template_total = "Total {measure} across all {pluralize(dimension1)} is {total}.",
    template_outlier = "Outlying {dimension} by {measure} is {outlier_insight}.",
    template_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}.",
    template_outlier_l2 = "In {root_outlier_dimension}, significant {dimension_l2} by {measure} is {outlier_insight}.",
    template_outlier_l2_multiple = "In {root_outlier_dimension}, significant {pluralize(dimension_l2)} by {measure} are {outlier_insight}.",
    use_renviron = FALSE,
    return_data = FALSE,
    simplify = FALSE,
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

  narrative_total <- glue::glue(template_total)

  narrative <- list(narrative_total) %>%
    setNames(glue::glue("Total {measure}"))

  variables <- list(template_total = narrative_total,
                    measure = measure,
                    dimension1 <- dimension1,
                    total = total)

  for (dimension in dimensions) {

    output <- df %>%
      get_descriptive_outliers(
        dimension = dimension,
        measure = measure,
        coverage = coverage,
        coverage_limit = coverage_limit)

    # Outputting all to the global env
    n_outliers <- output$n_outliers
    outlier_dimensions <- output$outlier_dimensions
    outlier_values <- output$outlier_values
    outlier_values_p <- output$outlier_values_p

    outlier_insight <- list(
      outlier_dimensions, " (", format_number(outlier_values), ", ", outlier_values_p, ")"
    ) %>%
      purrr::pmap(paste0) %>%
      unlist() %>%
      toString()

    if (n_outliers > 1) {
      template_outlier_final <- template_outlier_multiple
    } else {
      template_outlier_final <- template_outlier
    }

    narrative_outlier_final <- glue::glue(template_outlier_final)

    variables <- append(variables, list(outlier_insight = outlier_insight))
    variables <- append(variables, list(template_outlier_final = glue::glue(narrative_outlier_final)), 1)

    narrative <- list(narrative_outlier_final) %>%
      setNames(glue::glue("{dimension} by {measure}")) %>%
      append(narrative, after = 0)

    # Detailed Narrative: getting one level deeper into the outlying dimension
    if (narration_depth > 1 & dimension == dimension1 & length(dimensions) > 1) {

      root_outlier_dimensions <- outlier_dimensions

      for (i in seq_along(root_outlier_dimensions)) {

        root_outlier_dimension <- root_outlier_dimensions[i]
        dimension_l2 <- dimensions[which(dimensions == dimension) + 1]

        output <- df %>%
          dplyr::ungroup() %>%
          dplyr::filter(base::get(dimension) %in% root_outlier_dimensions[i]) %>%
          dplyr::select(-dplyr::all_of(dimension)) %>%
          get_descriptive_outliers(
            dimension = dimension_l2,
            measure = measure,
            coverage = coverage,
            coverage_limit = coverage_limit)

        n_outliers <- output$n_outliers
        outlier_dimensions <- output$outlier_dimensions
        outlier_values <- output$outlier_values
        outlier_values_p <- output$outlier_values_p

        outlier_insight <- list(outlier_dimensions, " (", format_number(outlier_values), ", ", outlier_values_p, ")") %>%
          purrr::pmap(paste0) %>%
          unlist() %>%
          toString()

        if (n_outliers > 1) {
          template_outlier_l2_final <- template_outlier_l2_multiple
        } else {
          template_outlier_l2_final <- template_outlier_l2
        }

        narrative_outlier_l2 <- glue::glue(template_outlier_l2_final)

        narrative <- list(narrative_outlier_l2) %>%
          setNames(glue::glue("{root_outlier_dimension} by {dimension_l2}")) %>%
          append(narrative, after = 0)

      }
    }
  }

  variables <- append(variables, list(narrative = narrative), 1)

  if (return_data == TRUE) {
    return(variables)
  }

  if (simplify == TRUE) {
    narrative = as.character(narrative)
  }

  return(narrative)
}


#' Create a list with descriptive outliers
#'
#' @param df Data frame of tibble, can be aggregated or raw
#' @param measure Numeric measure column
#' @param dimension Dimension within which the outlying patterns should be found
#' @param coverage Portion of variability to be covered by narrative, 0 to 1
#' @param coverage_limit Maximum number of elements to be narrated, overrides
#' coverage to avoid extremely verbose narrative creation
#'
#' @noRd
get_descriptive_outliers <- function(
    df,
    dimension,
    measure,
    coverage = 0.5,
    coverage_limit = 5) {

  table <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(dimension))) %>%
    dplyr::summarise(!!measure := sum(base::get(measure), na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(base::get(measure))) %>%
    dplyr::mutate(share = base::get(measure)/sum(base::get(measure))) %>%
    dplyr::mutate(cum_share = cumsum(share)) %>%
    dplyr::filter(cumsum(dplyr::lag(cum_share >= coverage, default = FALSE)) == 0) %>%
    dplyr::slice(1:coverage_limit)

  # For a single dimension we skip to the next level
  if (nrow(table) == 1 & table$cum_share[1] == 1) next

  n_outliers <- nrow(table)

  outlier_dimensions <- table %>%
    dplyr::select(dplyr::all_of(dimension)) %>%
    as.matrix() %>%
    as.character()

  outlier_values <- table %>%
    dplyr::select(dplyr::all_of(measure)) %>%
    as.matrix() %>%
    as.numeric()

  outlier_values_p <- table %>%
    dplyr::mutate(share = round(share * 100, 1)) %>%
    dplyr::select(share) %>%
    as.matrix() %>%
    as.numeric() %>%
    paste("%")

  output <- list(
    n_outliers = n_outliers,
    outlier_dimensions = outlier_dimensions,
    outlier_values = outlier_values,
    outlier_values_p = outlier_values_p
  )

  return(output)
}
