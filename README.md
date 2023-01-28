
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator

Template-based NLG framework for creating text narratives out of data

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
<!-- badges: end -->

## Installation

You can install the development version of narrator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("denisabd/narrator")
```

## Basic Use

``` r
library(narrator)

sales %>%
  narrate_desc(measure = "sales",
               dimensions = c("territory", "productline"))
#> sales across all territories is 10032628.85. Outlying territory by sales is EMEA (5 M, 49.6%). Outlying productline by sales is Classic Cars (3.9 M, 39.1%).
```
