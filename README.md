
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator

Template-based NLG framework for creating text narratives out of data

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
[![Codecov test
coverage](https://codecov.io/gh/denisabd/narrator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/denisabd/narrator?branch=main)
[![R-CMD-check](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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
               dimensions = c("Territory", "Product"))
```

Sales across all Territories is 2.2 M. Outlying Territory by Sales is NA
(938,914.9, 42.2 %). Outlying Products by Sales are Product E (606,807,
27.3 %), Product B (415,404.5, 18.7 %).
