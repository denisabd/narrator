
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

Total Sales across all Territories is 2.2 M. Outlying Territories by
Sales: NA (938,914.9, 42.2 %), EMEA (662,430.6, 29.8 %). In NA,
significant Product by Sales: Product E (257,960.1, 27.5 %), Product B
(181,285.7, 19.3 %), Product F (142,396.1, 15.2 %) In EMEA, significant
Product by Sales: Product E (189,312.1, 28.6 %), Product B (110,542.2,
16.7 %), Product F (83,064.26, 12.5 %) Outlying Products by Sales:
Product E (606,807, 27.3 %), Product B (415,404.5, 18.7 %), Product F
(294,399, 13.2 %).
