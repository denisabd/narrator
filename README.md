
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator

Template-based NLG framework for creating text narratives out of data

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
[![Codecov test
coverage](https://codecov.io/gh/denisabd/narrator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/denisabd/narrator?branch=main)
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
  narrate_descriptive(measure = "Sales",
               dimensions = c("Territory", "Productline"))
#> Sales across all Territories is 10032628.85. Outlying Territory by Sales is EMEA (5 M, 49.6%). Outlying Productline by Sales is Classic Cars (3.9 M, 39.1%).
```
