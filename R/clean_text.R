#' Clean Text String
#'
#' @description Function cleans up the text string improving narration, removing excessive white spaces
#' @param text Text string for cleaning. Can contain multiple sentences.
#' @param upper Vector of words that need to be changed to uppercase in text
#' @param lower Vector of words that need to be changed to lowercase in text
#'
#' @return Text string
#' @export
#'
#' @examples
#' text <- "Similarly in 2020 the sum of spend increased by 15.4%  ( 4.3 % higher  than average)."
#' clean_text(text)
#' clean_text(" Total  is 12,300 Orders ( 23.5 % for East  ) ")
clean_text <- function(text,
                       upper = c("YTD", "PYTD"),
                       lower = c("vs", "br>", "h1>", "h2>", "h3>", "h4>", "h5>", "h6>", "b>")
) {

  #assertion
  text_processed <- stringr::str_squish(text)

  # helper to detect the upper case
  is_upper <- function(string) {
    return (all(grepl("[[:upper:]]", strsplit(string, "")[[1]])))
  }

  # Clean spaces before commas and after brackets ---------------------------
  text_processed <- stringr::str_replace_all(text_processed, paste0("\\( "), paste0("\\("))
  text_processed <- stringr::str_replace_all(text_processed, paste0(" \\)"), paste0("\\)"))
  text_processed <- stringr::str_replace_all(text_processed, " ,", ",")
  text_processed <- stringr::str_replace_all(text_processed, paste0(" \\."), paste0("\\."))

  # Add space before percentage ---------------------------------------------
  text_processed <- stringr::str_replace_all(text_processed,"(?<=\\d)\\%", " %")

  # Capitalize first word ---------------------------------------------------
  # Split to sentences and capitalize every first word (if it is not UPPER case)
  sentences <- stringr::str_split(text_processed, "\\. ", simplify = F)

  for (i in 1:length(sentences[[1]])) {
    # check for UPPER case
    if (!is_upper(stringr::word(sentences[[1]][i]))) {
      sentences[[1]][i] <- stringr::str_replace_all(
        sentences[[1]][i],
        stringr::word(sentences[[1]][i]),
        stringr::str_to_title(stringr::word(sentences[[1]][i]))
      )
    }
  }

  text_processed <- paste(sentences[[1]], collapse = ". ")

  # Replace the pattern Lowercase
  for (i in seq_along(lower)) {
    text_processed <- stringr::str_replace_all(
      text_processed,
      stringr::fixed(lower[i], ignore_case = TRUE),
      tolower(lower[i]))
  }

  # Replace the pattern Uppercase
  for (i in seq_along(upper)) {
    text_processed <- stringr::str_replace_all(
      text_processed,
      stringr::fixed(upper[i], ignore_case = TRUE),
      toupper(upper[i]))
  }

  # Replace excessive whitespaces once again - in case if replacement led to multiple spaces
  text_processed <- stringr::str_squish(text_processed)

  return(text_processed)
}
