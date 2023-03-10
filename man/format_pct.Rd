% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_pct.R
\name{format_pct}
\alias{format_pct}
\title{Add HTML Tags with Colors to Percentage Values in Text}
\usage{
format_pct(text, positive = "green", negative = "red")
}
\arguments{
\item{text}{Text string or character vector/list of text strings}

\item{positive}{Color to highlight percentage increase}

\item{negative}{Color to highlight percentage decline}
}
\value{
\code{\link[=list]{list()}} of text strings
}
\description{
Add HTML Tags with Colors to Percentage Values in Text
}
\examples{
text <- "Spend increased by 13.2 \% in EMEA but decreased by -13.2 \% in LATAM"
format_pct(text)
}
