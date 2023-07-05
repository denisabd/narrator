#' Format the Narrative with HTML tags for Interactive Reports and Applications
#'
#' @param narrative List of narratives returned by narrate_* function
#' @param format_percentage Add percentage formatting to text
#' @param title_tag HTML tag to be added to the names of the narrative
#' @param ... Other arguments passed to `format_pct()` function
#'
#' @return html [character()] of text strings
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' sales %>%
#' group_by(Region, Product) %>%
#'   summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
#'   arrange(desc(Sales)) %>%
#'   narrate_descriptive() %>%
#'   to_html()
to_html <- function(
    narrative,
    format_percentage = TRUE,
    title_tag = "h4",
    ...) {

  if (!class(narrative) %in% c("list", "character")) stop("narrative must be a list or character")
  if (length(narrative) < 1) stop("narrative must me a list of length 1+")

  if (format_percentage == TRUE) {
    narrative <- narrator::format_pct(narrative, ...)
  }

  if ("list" %in% class(narrative)) {
    names(narrative) <- narrator::add_tag(names(narrative), title_tag)

    narrative <- unlist(narrative)

    narrative <- mapply(c, names(narrative), as.character(narrative)) %>%
      as.character()
  }

  # replacing /n/n with the <br> tag to make it work with HTML
  narrative <- stringr::str_replace_all(narrative, "\\\n\\\n", "<br><br>")

  narrative <- narrative %>%
    htmltools::HTML()

  return(narrative)
}
