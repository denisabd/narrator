#' Add HTML tags to text
#'
#' @param text Text string
#' @param tag HTML tag like p, b, h1 or other
#'
#' @return Text string with HTML tags
#' @export
#'
#' @examples add_tag("Title Text", tag = "h2")
#' add_tag("bold text", tag = "b")
add_tag <- function(text, tag = "h3") {

  # Check if text or number is provided for text
  if (!is.character(text) && !is.numeric(text)) stop("Provide text string or numeric value")

  output <- glue::glue("<{tag}> {text} <{tag}>")
  return(output)
}
