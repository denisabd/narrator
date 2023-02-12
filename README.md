
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
#> Total Sales across all Regions is 35.7 M.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (16.9 M, 47.3 %), EMEA (12.5 M, 34.8 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food & Beverage (6.1 M, 35.9 %), Electronics (3.3 M, 19.2 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food & Beverage (4.3 M, 34.9 %), Electronics (2.6 M, 20.8 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food & Beverage (12.7 M, 35.5 %), Electronics (7.2 M, 20.1 %).
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
| NA     | 16915195 | 0.473 |
| EMEA   | 12450534 | 0.348 |
| ASPAC  |  3242657 | 0.091 |
| LATAM  |  3121094 | 0.087 |
| Total  | 35729480 | 0.999 |

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
| Food & Beverage | 12669874 |
| Electronics     |  7193458 |
| Home            |  5313994 |
| Tools           |  5275770 |
| Baby            |  2836899 |
| Clothing        |  2439485 |
| Total           | 35729480 |
