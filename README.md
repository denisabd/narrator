
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
  group_by(Region) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Sales)) %>%
  mutate(Share = round(Sales/sum(Sales), 3)) %>%
  janitor::adorn_totals() %>%
  kable()
```

| Region |    Sales | Share |
|:-------|---------:|------:|
| NA     | 18079736 | 0.466 |
| EMEA   | 13555413 | 0.349 |
| ASPAC  |  3919261 | 0.101 |
| LATAM  |  3236068 | 0.083 |
| Total  | 38790478 | 0.999 |

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
| Food & Beverage | 15543470 |
| Electronics     |  8608963 |
| Home            |  4599371 |
| Tools           |  4404197 |
| Baby            |  3256835 |
| Clothing        |  2377643 |
| Total           | 38790478 |
