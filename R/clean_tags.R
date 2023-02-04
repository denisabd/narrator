#' Clean HTML Tags from the Text
#'
#' @param html_string Text with html tags
#'
#' @return [character()] vector with text strings without tags
#' @export
#'
#' @seealso You can add tags to text using [add_tag()] function
#'
#' @examples
#' clean_tags("<b>Total increase is equal to 14.5 % </b>")
#' clean_tags("<h2>Sales by Region</h3>")
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
