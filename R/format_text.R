#' Add HTML tags to colorize and bold the string of text
#'
#' @description Function adds html code around the selected string adding options for colorized and/or bold display in HTML documents.
#' @param text Text string that you want to format
#' @param color Color name, for "auto" the color will be determined based on the number parsed - red for negative, green for positive
#' @param bold Make text bold or not
#'
#' @return text with HTML tags
#' @export
#'
#' @examples
#' text <- format_text("1.2%", color = "auto", bold = TRUE)
#' text
format_text <- function(text, color = "auto", bold = TRUE) {

  text <- as.character(text)
  text <- stringr::str_replace_all(text, "  "," ")

  # automatic color will, if string contains a number
  if (color == "auto" && grepl("\\d", text)== TRUE) {

    color <- dplyr::case_when(
      readr::parse_number(text) < 0 ~ "red",
      readr::parse_number(text) > 0 ~ "green",
      TRUE ~ "black")
  } else if (color == "auto" && grepl("\\d", text) == FALSE) {
    color <- "black"
  }

  # bold text
  if (bold == TRUE) {
    text_processed <- sprintf("<b> <span style='color: %s;'>%s</span> </b>", color, text)
  } else {
    text_processed <- sprintf("<span style='color: %s;'>%s</span>", color, text)
  }

  return(text_processed)

}
