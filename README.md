
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

## Basic Use

``` r
library(narrator)
library(dplyr)
library(knitr)

sales %>%
  narrate_descriptive(measure = "Sales",
               dimensions = c("Region", "Product"))
#> $`Total Sales`
#> Total Sales across all Regions is 35.4 M.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (16.4 M, 46.2 %), EMEA (12.4 M, 35 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food and Beverage (5.6 M, 34.3 %), Electronics (3.1 M, 18.8 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food and Beverage (4.4 M, 35.8 %), Electronics (2.3 M, 18.8 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food and Beverage (12.4 M, 35.1 %), Electronics (6.6 M, 18.8 %).
```

``` r
sales %>%
  group_by(Region) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Sales)) %>%
  mutate(Share = round(Sales/sum(Sales), 3)) %>%
  janitor::adorn_totals() %>%
  kable()
```

| Region |    Sales | Share |
|:-------|---------:|------:|
| NA     | 16359589 | 0.462 |
| EMEA   | 12376976 | 0.350 |
| ASPAC  |  3575921 | 0.101 |
| LATAM  |  3080286 | 0.087 |
| Total  | 35392772 | 1.000 |

``` r
sales %>%
  group_by(Product) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Sales)) %>%
  janitor::adorn_totals() %>%
  kable()
```

| Product           |    Sales |
|:------------------|---------:|
| Food and Beverage | 12411093 |
| Electronics       |  6639429 |
| Tools             |  5534432 |
| Home              |  5266162 |
| Baby              |  3109344 |
| Clothing          |  2432313 |
| Total             | 35392772 |
