#' Clean text string
#'
#' @description Function cleans up the text string improving narration, removing excessive white spaces
#' @param txt Text string for cleaning. Can contain multiple sentences.
#' @param upper Vector of words that need to be changed to uppercase in text
#'
#' @return Text string
#' @export
#'
#' @examples
#' txt <- "Similarly in 2020 the sum of spend increased by 15.4%  ( 4.3 % higher  than averag)."
#' clean_text(txt)
#' clean_text(" Total count of   po_order_id is 12300  ")
#' clean_text("  Sum of Spend  is equal to 15.4 M")
#' clean_text("  sum of invoice_amt_usd  is equal to 15.4 M")
#' clean_text("Similarly in 2020 the sum of spend increased by 15.4%  ( 4.3 % higher  than averag).")
#' clean_text(" Mean cycle_time is equal to 3.4  days")
#' clean_text(" count of req_requisition_id is equal to 2400.")
clean_text <- function(txt,
                       upper = c("YTD", "PYTD"),
                       lower = c("vs", "br", "h1", "h2", "h3", "h4", "h5", "h6", "b")
) {

  #assertion
  txt_processed <- stringr::str_squish(txt)

  # helper to detect the upper case
  is_upper <- function(string) {
    return (all(grepl("[[:upper:]]", strsplit(string, "")[[1]])))
  }

  # Clean spaces before commas and after brackets ---------------------------
  txt_processed <- stringr::str_replace_all(txt_processed, paste0("\\( "), paste0("\\("))
  txt_processed <- stringr::str_replace_all(txt_processed, paste0(" \\)"), paste0("\\)"))
  txt_processed <- stringr::str_replace_all(txt_processed, " ,", ",")
  txt_processed <- stringr::str_replace_all(txt_processed, paste0(" \\."), paste0("\\."))

  # Add space before percentage ---------------------------------------------
  txt_processed <- stringr::str_replace_all(txt_processed,"(?<=\\d)\\%", " %")

  # Capitalize first word ---------------------------------------------------
  # Split to sentences and capitalize every first word (if it is not UPPER case)
  sentences <- stringr::str_split(txt_processed, "\\. ", simplify = F)

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

  txt_processed <- paste(sentences[[1]], collapse = ". ")

  # Replace the pattern Lowercase
  for (i in seq_along(lower)) {
    txt_processed <- stringr::str_replace_all(
      txt_processed,
      stringr::fixed(lower[i], ignore_case = TRUE),
      tolower(lower[i]))
  }

  # Replace the pattern Uppercase
  for (i in seq_along(upper)) {
    txt_processed <- stringr::str_replace_all(
      txt_processed,
      stringr::fixed(upper[i], ignore_case = TRUE),
      toupper(upper[i]))
  }

  # Replace excessive whitespaces once again - in case if replacement led to multiple spaces
  txt_processed <- stringr::str_squish(txt_processed)

  return(txt_processed)
}
