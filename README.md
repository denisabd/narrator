
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
#> Total Sales across all Regions is 35.5 M.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (16.4 M, 46 %), EMEA (12.9 M, 36.4 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food & Beverage (5.8 M, 35.7 %), Electronics (3.4 M, 20.7 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food & Beverage (4.7 M, 36.6 %), Electronics (2.5 M, 19.2 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food & Beverage (12.6 M, 35.5 %), Electronics (7.2 M, 20.3 %).
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
| NA     | 16351889 | 0.460 |
| EMEA   | 12918195 | 0.364 |
| ASPAC  |  3450130 | 0.097 |
| LATAM  |  2817267 | 0.079 |
| Total  | 35537481 | 1.000 |

``` r
sales %>%
  group_by(Product) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Sales)) %>%
  janitor::adorn_totals() %>%
  kable()
```

| Product         |    Sales |
|:----------------|---------:|
| Food & Beverage | 12613552 |
| Electronics     |  7207901 |
| Home            |  5172389 |
| Tools           |  5135457 |
| Baby            |  2883246 |
| Clothing        |  2524937 |
| Total           | 35537481 |
