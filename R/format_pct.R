#' Add HTML Tags with Colors to Percentage Values in Text
#'
#' @param text Text string or character vector/list of text strings
#' @param positive Color to highlight percentage increase
#' @param negative Color to highlight percentage decline
#'
#' @return [list()] of text strings
#' @export
#'
#' @examples
#' text <- "Spend increased by 13.2 % in EMEA but decreased by -13.2 % in LATAM"
#' format_pct(text)
format_pct <- function(text,
                       positive = "green",
                       negative = "red") {


  # Check if text or number is provided for text
  if (!is.character(text) && !is.numeric(text) && !is.list(text)) {
    stop("Provide list, character text or numeric value")
  }

  # Replace excessive punctuation
  text <- lapply(text, stringr::str_replace_all, " %", "%")
  text <- lapply(text, stringr::str_replace_all, "\\(", " \\(")
  text <- lapply(text, stringr::str_replace_all, "\\)", " \\)")
  text <- lapply(text, stringr::str_replace_all, "(?![.%-//)//://(])[[:punct:]]", "")

  # looping through the vector/list of narrations
  for (n in seq_along(text)) {

    text[n] <- text[n] %>%
      stringr::str_split(" ") %>%
      magrittr::extract2(1) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        num_value = ifelse(stringr::str_detect(value, "%"), readr::parse_number(value), NA),
        value = dplyr::case_when(
          num_value >= 0 ~  format_text(value, color = positive),
          num_value < 0 ~ format_text(value, color = negative),
          TRUE ~ value)
      ) %>%
      dplyr::select(value) %>%
      as.matrix() %>%
      as.character() %>%
      paste(collapse = " ") %>%
      suppressWarnings()
  }

  if ("list" %in% class(text)) {
    text_output <- text %>%
      lapply(clean_text)
  } else {
    text_output <- text %>%
      vapply(clean_text)
  }

  return(text_output)
}
