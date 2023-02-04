
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
#> Total Sales across all Regions: 2.1 M.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (1 M, 47.4 %), EMEA (655.4 K, 30.7 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Product E (241.6 K, 23.9 %), Product B (201.9 K, 20 %), Product F (154.1 K, 15.2 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Product E (198.6 K, 30.3 %), Product B (95.9 K, 14.6 %), Product F (92.9 K, 14.2 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Product E (546.9 K, 25.6 %), Product B (396.7 K, 18.6 %), Product F (326.2 K, 15.3 %).
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

| Region |     Sales | Share |
|:-------|----------:|------:|
| NA     | 1011609.9 | 0.474 |
| EMEA   |  655361.4 | 0.307 |
| ASPAC  |  286822.2 | 0.134 |
| LATAM  |  181072.7 | 0.085 |
| Total  | 2134866.2 | 1.000 |

``` r
sales %>%
  group_by(Product) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Sales)) %>%
  janitor::adorn_totals() %>%
  kable()
```

| Product   |      Sales |
|:----------|-----------:|
| Product E |  546888.88 |
| Product B |  396713.90 |
| Product F |  326244.84 |
| Product H |  262662.94 |
| Product I |  215874.38 |
| Product C |  196840.76 |
| Product A |  100033.10 |
| Product D |   89607.44 |
| Total     | 2134866.24 |
