
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator <img src="man/figures/hex.png" align="right" alt="" width="160" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
[![Codecov test
coverage](https://codecov.io/gh/denisabd/narrator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/denisabd/narrator?branch=main)
[![R-CMD-check](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Template-based NLG framework for creating text narratives out of data.
Demo [shiny application](https://deny.shinyapps.io/narrator_app/)
showing core package capabilities is deployed on shinyapps.io.

## Installation

You can install the development version of narrator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("denisabd/narrator")
```

## Basic Use Cases

``` r
library(narrator)
library(dplyr)
library(knitr)

sales %>%
  narrate_descriptive(
    measure = "Sales",
    dimensions = c("Region", "Product"))
#> $`Total Sales`
#> Total Sales across all Regions is 38.79 M.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (18.1 M, 46.6 %), EMEA (13.6 M, 34.9 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food & Beverage (7.4 M, 40.9 %), Electronics (3.8 M, 21 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food & Beverage (5.3 M, 38.8 %), Electronics (3.2 M, 23.5 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food & Beverage (15.5 M, 40.1 %), Electronics (8.6 M, 22.2 %).
```

``` r
sales %>%
  narrate_trend(
    measure = "Sales",
    date = "Date",
    dimensions = c("Region", "Product")
  )
#> $`2021 YTD vs 2020 YTD`
#> From 2020 YTD to 2021 YTD, Sales had an increase of 1.13 M (9.1 %, 12.42 M to 13.55 M).
#> 
#> $`Sales change by Region`
#> Regions with biggest changes of Sales are NA (533.1 K, 9.1 %, 5.9 M to 6.4 M), EMEA (416.9 K, 9.91 %, 4.2 M to 4.6 M).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales change are Food & Beverage (243.3 K, 9.92 %, 2.5 M to 2.7 M), Tools (190.5 K, 32.72 %, 582.2 K to 772.7 K).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales change are Electronics (313.1 K, 36.05 %, 868.6 K to 1.2 M), Food & Beverage (244.8 K, 15.01 %, 1.6 M to 1.9 M).
#> 
#> $`Sales change by Product`
#> Products with biggest changes of Sales are Food & Beverage (535.4 K, 10.63 %, 5 M to 5.6 M), Electronics (525.9 K, 19.79 %, 2.7 M to 3.2 M).
```
