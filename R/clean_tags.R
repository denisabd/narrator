#' Clean html tags from the text
#'
#' @param html_string Text with html tags
#'
#' @return Text string without tags
#' @export
#'
#' @examples
#' clean_tags("<b class = ttip>Total increase is equal to 14.5 % <span class='tooltiptext'> from 1300 to 1670</span> </b>")
clean_tags <- function(html_string) {

  # Text string is required
  if (!is.character(html_string)) stop("Provide text string")

  string_processed <- gsub("<.*?>", "", html_string) %>%
    stringr::str_replace_all(paste0("\\( "), paste0("\\(")) %>%
    stringr::str_replace_all(paste0(" \\)"), paste0("\\)")) %>%
    stringr::str_replace_all(" ,", ",") %>%
    stringr::str_replace_all(paste0(" \\."), paste0("\\.")) %>%
    stringr::str_replace_all("(?<=\\d)\\%", " %") %>%
    stringr::str_squish()

  return(string_processed)
}
