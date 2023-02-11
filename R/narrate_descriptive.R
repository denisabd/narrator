#' Create Descriptive Text Narratives from Data
#'
#' @description `narrate_descriptive()` creates text narratives from a data frame containing one numeric and one or more character or factor text columns
#' using \code{\link[glue]{glue}} syntax. Function can work with raw or aggregated data frame.
#' It will automatically use the first numeric column as \code{measure}
#' and all character or factor columns as \code{dimensions}
#'
#' @param df [data.frame()] or [tibble()] Data frame of tibble, can be aggregated or raw
#' @param measure Numeric measure for function to create calculations with,
#' if NULL then it will take the first numeric field available
#' @param dimensions Vector of dimensions for analysis, by default all character
#' or factor variable will be used
#' @param summarization Approach for data summarization/aggregation - 'sum', 'count' or 'average'
#' @param coverage Numeric portion of variability to be covered by narrative, 0 to 1
#' @param coverage_limit Integer maximum number of elements to be narrated, overrides
#' coverage to avoid extremely verbose narrative creation
#' @param narration_depth Parameter to control the depth of the analysis 1 for summary and 2 for detailed
#' @param template_total \code{\link[glue]{glue}} template for total volumes narrative
#' @param template_average \code{\link[glue]{glue}} template for average volumes narrative
#' @param template_outlier \code{\link[glue]{glue}} template for single outlier narrative
#' @param template_outlier_multiple \code{\link[glue]{glue}} template for multiple outliers narrative
#' @param template_outlier_l2 \code{\link[glue]{glue}} template for deeper hierarchical single outlier narrative
#' @param template_outlier_l2_multiple \code{\link[glue]{glue}} template for deeper hierarchical multiple outliers narrative
#' @param use_renviron If [TRUE] use .Renviron variables in the template
#' @param return_data If [TRUE] - return a list of variables used in the function's templates
#' @param simplify If [TRUE] - return a character vector, if [FALSE] - named list
#' @param format_numbers If [TRUE] - format big numbers to K/M/B using [format_num()] function
#' @param ... other arguments passed to \code{\link[glue]{glue}}
#'
#' @importFrom rlang :=
#' @importFrom tidyselect where
#' @importFrom utils head
#'
#' @return A [tibble()] by default and [character()] if `simplify = TRUE`
#' @export
#'
#' @examples
#' sales %>%
#' narrate_descriptive(measure = "Sales",
#'             dimensions = c("Region", "Product"))
#'
#' sales %>%
#'   dplyr::filter(Product %in% c("Product A", "Product B")) %>%
#'   dplyr::group_by(Product, Region)  %>%
#'   dplyr::summarise(Quantity = sum(Quantity)) %>%
#'   narrate_descriptive()
#'
#' sales %>%
#' narrate_descriptive(measure = "Order ID", dimensions = "Region", summarization = "count")
narrate_descriptive <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    summarization = "sum",
    coverage = 0.5,
    coverage_limit = 5,
    narration_depth = 2,
    template_total = "Total {measure} across all {pluralize(dimension_one)}: {total}.",
    template_average = "Average {measure} across all {pluralize(dimension_one)}: {total}.",
    template_outlier = "Outlying {dimension} by {measure} is {outlier_insight}.",
    template_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}.",
    template_outlier_l2 = "In {level_l1}, significant {level_l2} by {measure} is {outlier_insight}.",
    template_outlier_l2_multiple = "In {level_l1}, significant {pluralize(level_l2)} by {measure} are {outlier_insight}.",
    use_renviron = FALSE,
    return_data = FALSE,
    simplify = FALSE,
    format_numbers = TRUE,
    ...) {


  # Assertion ---------------------------------------------------------------
  if (!is.data.frame(df) & !dplyr::is.tbl(df)) stop("'df' must be a data frame, tibble, or dplyr::tbl connection")

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
  dimension_dtypes <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(dimensions)) %>%
    head() %>%
    dplyr::collect() %>%
    lapply(class)

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

  if (!class(df[[measure]]) %in% c("numeric", "integer", "character", "factor")) {
    stop(glue::glue("{measure} must be a numeric column, but is {class(df[[measure]])}"))
  }

  # Renviron ----------------------------------------------------------------
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

  # Total Narrative ---------------------------------------------------------
  dimension_one <- dimensions[1]

  total <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(!!measure := switch(
      summarization,
      "average" = mean(base::get(measure), na.rm = TRUE),
      "sum" = sum(base::get(measure), na.rm = TRUE),
      "count" = dplyr::n_distinct(base::get(measure), na.rm = TRUE)
    )
    ) %>%
    as.matrix() %>%
    as.numeric() %>%
    round(1)

  if (format_numbers == TRUE) total <- format_num(total)

  # Sum/Count ---------------------------------------------------------------
  if (summarization %in% c("sum", "count")) {
    narrative_total <- glue::glue(template_total)

    narrative <- list(narrative_total) %>%
      rlang::set_names(glue::glue("Total {measure}"))

    variables <- list(
      list(
        narrative_total = narrative_total,
        template_total = template_total,
        measure = measure,
        dimension_one = dimension_one,
        total = total)
    ) %>%
      rlang::set_names(glue::glue("Total {measure}"))

    # Average ------------------------------------------------------------
  } else if (summarization == "average") {
    narrative_average <- glue::glue(template_average)

    narrative <- list(narrative_average) %>%
      rlang::set_names(glue::glue("Average {measure}"))

    variables <- list(
      list(
        narrative_average = narrative_average,
        template_average = template_average,
        measure = measure,
        dimension_one = dimension_one,
        total = total)
    ) %>%
      rlang::set_names(glue::glue("Average {measure}"))
  }

  # High-Level Narrative ---------------------------------------------------------
  for (dimension in dimensions) {

    output <- df %>%
      get_descriptive_outliers(
        dimension = dimension,
        measure = measure,
        summarization = summarization,
        coverage = coverage,
        coverage_limit = coverage_limit)

    if (is.null(output)) next

    # Outputting all to the global env
    n_outliers <- output$n_outliers
    outlier_levels <- output$outlier_levels
    outlier_values <- output$outlier_values
    outlier_values_p <- output$outlier_values_p

    if (format_numbers == TRUE) {
      outlier_values <- format_num(outlier_values)
    }

    outlier_insight <- list(
      outlier_levels, " (", outlier_values, ", ", outlier_values_p,
      ifelse(
        summarization == "average",
        glue::glue(" vs average {measure})"),
        ")"
      )
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

    variables_l1 <- list(
      list(
        narrative_outlier_final = narrative_outlier_final,
        template_outlier_final = template_outlier_final,
        dimension = dimension,
        measure = measure,
        outlier_insight = outlier_insight,
        n_outliers = n_outliers,
        outlier_levels = outlier_levels,
        outlier_values = outlier_values,
        outlier_values_p = outlier_values_p
      )
    ) %>%
      rlang::set_names(glue::glue("{dimension} by {measure}"))

    variables <- append(variables, variables_l1)

    narrative <- list(narrative_outlier_final) %>%
      rlang::set_names(glue::glue("{dimension} by {measure}")) %>%
      append(narrative, after = 0)

    # Detailed Narrative ------------------------------------------------------
    # Getting one level deeper into the outlying dimension
    if (narration_depth > 1 & dimension == dimension_one & length(dimensions) > 1) {

      levels_l1 <- outlier_levels

      for (i in seq_along(levels_l1)) {

        level_l1 <- levels_l1[i]
        level_l2 <- dimensions[which(dimensions == dimension) + 1]

        output <- df %>%
          dplyr::ungroup() %>%
          dplyr::filter(base::get(dimension) %in% levels_l1[i]) %>%
          dplyr::select(-dplyr::all_of(dimension)) %>%
          get_descriptive_outliers(
            dimension = level_l2,
            measure = measure,
            summarization = summarization,
            coverage = coverage,
            coverage_limit = coverage_limit)

        if (is.null(output)) next

        n_outliers <- output$n_outliers
        outlier_levels <- output$outlier_levels
        outlier_values <- output$outlier_values
        outlier_values_p <- output$outlier_values_p

        if (format_numbers == TRUE) {
          outlier_values <- format_num(outlier_values)
        }

        outlier_insight <- list(
          outlier_levels, " (", outlier_values, ", ", outlier_values_p,
          ifelse(
            summarization == "average",
            glue::glue(" vs average {measure})"),
            ")"
          )
        ) %>%
          purrr::pmap(paste0) %>%
          unlist() %>%
          toString()

        if (n_outliers > 1) {
          template_outlier_l2_final <- template_outlier_l2_multiple
        } else {
          template_outlier_l2_final <- template_outlier_l2
        }

        narrative_outlier_l2 <- glue::glue(template_outlier_l2_final)

        variables_l2 <- list(
          list(
            narrative_outlier_l2_final = narrative_outlier_l2,
            template_outlier_l2_final = template_outlier_l2,
            level_l1 = level_l1,
            level_l2 = level_l2,
            measure = measure,
            outlier_insight = outlier_insight,
            n_outliers = n_outliers,
            outlier_levels = outlier_levels,
            outlier_values = outlier_values,
            outlier_values_p = outlier_values_p
          )
        ) %>%
          rlang::set_names(level_l1)

        variables <- append(variables, variables_l2)

        narrative <- list(narrative_outlier_l2) %>%
          rlang::set_names(glue::glue("{level_l1} by {level_l2}")) %>%
          append(narrative, after = 0)
      }
    }
  }

  variables <- append(variables, list(narrative = narrative), 0)

  if (return_data == TRUE) {
    return(variables)
  }

  if (simplify == TRUE) {
    narrative <- as.character(narrative)
    variables$narrative <- as.character(variables$narrative)
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
    summarization = "sum",
    coverage = 0.5,
    coverage_limit = 5) {

  table <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(dimension))) %>%
    dplyr::summarise(!!measure := switch(
      summarization,
      "sum" = sum(base::get(measure), na.rm = TRUE),
      "count" = dplyr::n_distinct(base::get(measure), na.rm = TRUE),
      "average" = mean(base::get(measure), na.rm = TRUE)
    )
    ) %>%
    dplyr::arrange(dplyr::desc(base::get(measure)))

  if (summarization %in% c("sum", "count")) {

    table <- table %>%
      dplyr::mutate(share = base::get(measure)/sum(base::get(measure))) %>%
      dplyr::mutate(cum_share = cumsum(share)) %>%
      dplyr::filter(cumsum(dplyr::lag(cum_share >= coverage, default = FALSE)) == 0) %>%
      dplyr::slice(1:coverage_limit)

    # For a single dimension we skip to the next level
    if (nrow(table) == 1 & table$cum_share[1] == 1) return(NULL)

  } else if (summarization %in% c("average")) {

    table <- table %>%
      dplyr::mutate(share = base::get(measure)/mean(base::get(measure)) - 1) %>%
      dplyr::arrange(dplyr::desc(abs(share))) %>%
      dplyr::mutate(cum_share = cumsum(abs(share))/(max(share) - min(share))) %>%
      dplyr::filter(cumsum(dplyr::lag(cum_share >= coverage*2, default = FALSE)) == 0) %>%
      dplyr::slice(1:coverage_limit)
  }

  n_outliers <- nrow(table)

  outlier_levels <- table %>%
    dplyr::select(dplyr::all_of(dimension)) %>%
    as.matrix() %>%
    as.character()

  outlier_values <- table %>%
    dplyr::select(dplyr::all_of(measure)) %>%
    as.matrix() %>%
    as.numeric() %>%
    round(1)

  outlier_values_p <- table %>%
    dplyr::mutate(share = round(share * 100, 1)) %>%
    dplyr::select(share) %>%
    as.matrix() %>%
    as.numeric() %>%
    paste("%")

  output <- list(
    n_outliers = n_outliers,
    outlier_levels = outlier_levels,
    outlier_values = outlier_values,
    outlier_values_p = outlier_values_p
  )

  return(output)
}
