#' Format numeric value
#'
#' @param num Number or numeric vector
#' @param format Format for numeric output - "auto", "K", "M", "B" or "T"
#' @param decimals Number of decimal numbers for rounding
#' @param threshold Threshold for converting to a specific format, below threshold only thousand delimiters will be added
#' @param format_list List of formats to use for automated
#'
#' @return vector of numerics
#' @export
#'
#' @examples format_num(27334254)
#' format_num(c(27334254, 12332, 23425435534))
format_num <- function(
    num,
    format = "auto",
    decimals = 1,
    threshold = 10000,
    format_list = c("", "K", "M", "B", "T")) {

  if (!is.numeric(num)) {
    stop("Argument num must be a numeric vector")
  }

  format <- toupper(format)

  # Return simple formatting if num is below the threshold
  if (suppressWarnings(max(abs(num))) < threshold) {
    num_formatted <- prettyNum(as.numeric(num), big.mark = ",")
    return(num_formatted)
  }

  # Formatting
  if (format == "AUTO") {

    div <- findInterval(as.numeric(gsub("\\,", "", abs(num))), c(0, 1e3, 1e6, 1e9, 1e12))

    format <- format_list[div]

    num_formatted <- trimws(paste(round(as.numeric(gsub("\\,", "", num))/10^(3*(div-1)), decimals), format))

  } else {

    num_formatted <- dplyr::case_when(format == "T" ~ paste(round(num/1e12, decimals), format),
                                      format == "B" ~ paste(round(num/1e9, decimals), format),
                                      format == "M" ~ paste(round(num/1e6, decimals), format),
                                      format == "K" ~ paste(round(num/1e3, decimals), format),
                                      TRUE ~ prettyNum(as.numeric(num), big.mark = ",")
    )
  }

  return(num_formatted)
}
