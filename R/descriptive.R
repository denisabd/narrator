#' Create Descriptive Narrative
#'
#' @param df
#' @param measure
#' @param dimensions
#' @param coverage
#' @param coverage_limit
#' @param narrative_total
#' @param narrative_outlier
#' @param narrative_outlier_multiple
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
narrate_desc <- function(
    df,
    measure = NULL,
    dimensions = NULL,
    coverage = 0.5,
    coverage_limit = 5,
    narrative_total = "{measure} across all {pluralize(dimension1)} is {total}. ",
    narrative_outlier = "Outlying {dimension} by {measure} is {outlier_insight}. ",
    narrative_outlier_multiple = "Outlying {pluralize(dimension)} by {measure} are {outlier_insight}. ",
    ...) {


  if (is.null(dimensions)) {
    dimensions <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(where(is.character), where(is.factor)) %>%
      names()
  }

  if (length(dimensions) < 1) stop("Desciptive narrative requires at least one dimension")

  if (is.null(measure)) {
    measure <- df %>%
      dplyr::ungroup() %>%
      dplyr::select_if(is.numeric) %>%
      names() %>%
      magrittr::extract2(1)
  }

  if (length(measure) == 0) stop("Desciptive narrative requires a measure")

  dimension1 <- dimensions[1]

  total <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(!!measure := sum(base::get(measure), na.rm = TRUE)) %>%
    as.matrix() %>%
    as.numeric()


  narrative <- glue::glue(narrative_total, ...)

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
      as.numeric()

    outlier_insight <- list(outlier_dimension, " (", format_number(outlier_value), ", ", outlier_value_p, "%)") %>%
      purrr::pmap(paste0) %>%
      unlist() %>%
      toString()

    if (n_outliers > 1) {
      narrative_outlier_final <- narrative_outlier_multiple
    } else {
      narrative_outlier_final <- narrative_outlier
    }


    narrative <- glue::glue(narrative,
                            "",
                            narrative_outlier_final,
                            ...)


    # if (dimension == dimension1 & length(dimensions) > 1) {

    #   df %>%
    #     dplyr::filter()
    # }



  }



  return(narrative)

}
