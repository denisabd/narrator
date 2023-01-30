#' Synthetic Sales Data
#'
#' The original data, from SWAPI, the Star Wars API, <https://swapi.dev/>, has been revised
#' to reflect additional research into gender and sex determinations of characters.
#'
#' @format A tibble with 1000 rows and 8 variables:
#' \describe{
#' \item{Order ID}{Unique ID for the order}
#' \item{Date}{Date of Order creation}
#' \item{Territory}{Territory/Region of shipment}
#' \item{Product}{Product Name}
#' \item{Promotion}{Was Promotion present}
#' \item{Price}{Raw unit price for quantity}
#' \item{Quantity}{Number of units sold}
#' \item{Sales}{Final Sales, including promotional discounts}
#' }
#' @examples
#' sales
"sales"
