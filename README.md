
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator <img src="man/figures/hex.png" align="right" width="160"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
[![Codecov test
coverage](https://codecov.io/gh/denisabd/narrator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/denisabd/narrator?branch=main)
[![R-CMD-check](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

Template-based NLG framework for creating text narratives out of data
and enhance them using ChatGPT. Demo [shiny
application](https://deny.shinyapps.io/narrator_app/) showing core
package capabilities is deployed on shinyapps.io.

## Installation

You can install the development version of narrator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("denisabd/narrator")
```

## Basic Use Cases

Simple tables with one or more categorical columns (dimensions) and one
measure can be transformed to text using `narrate_descriptive()`
function.

``` r
library(narrator)
library(dplyr)
```

``` r
narrative_one <- sales %>%
  narrate_descriptive(
    measure = "Sales",
    dimensions = c("Region", "Product")
  )

narrative_one
#> $`Total Sales`
#> Total Sales across all Regions is 38790478.4.
#> 
#> $`Region by Sales`
#> Outlying Regions by Sales are NA (18079736.4, 46.6 %), EMEA (13555412.7, 34.9 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food & Beverage (7392821, 40.9 %), Electronics (3789132.7, 21 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food & Beverage (5265113.2, 38.8 %), Electronics (3182803.4, 23.5 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food & Beverage (15543469.7, 40.1 %), Electronics (8608962.8, 22.2 %).
```

You can analyze changes over time using `narrate_trend()` function:

``` r
narrative_two <- sales %>%
  narrate_trend(
    measure = "Sales",
    date = "Date",
    dimensions = c("Region", "Product")
  )

narrative_two
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

## ChatGPT

`narrator` can use ChatGPT API to improve your narratives. To do so you
can either set `use_chatgpt = TRUE` in any function that creates
narrative or use `enhance_narrative()` to improve existing narrative
output. You can supply `list` or `character`, function will collapse all
text into a sentence and send a request to Chat GPT. Set your token in
`.Renviron` file as `OPENAI_API_KEY` or supply it to a function as
`openai_api_key` argument.

This functionality requires you to setup the ChatGPT API key and make it
accessible from R.

- Obtain your ChatGPT API key. You can create an API key by accessing
  [OpenAI API page](https://platform.openai.com/account/api-keys)

- [Best Practices for API Key
  Safety](https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety)

- Change your `.Renviron` file with `usethis::edit_r_environ()` by
  adding \``` OPENAI_API_KEY=xx-xxxxxx` ``

``` r
narrative_enhanced <- enhance_narrative(narrative_one)
cat(narrative_enhanced)
```

The total sales figure for all regions amounts to an impressive
38,790,478.4. The regions that stood out in terms of sales were North
America (18,079,736.4, 46.6%) and EMEA (13,555,412.7, 34.9%). In North
America, the products that contributed significantly to sales were Food
& Beverage (7,392,821, 40.9%) and Electronics (3,789,132.7, 21%).
Similarly, in EMEA, Food & Beverage (5,265,113.2, 38.8%) and Electronics
(3,182,803.4, 23.5%) played a significant role in boosting sales. The
products that performed exceptionally well across all regions were Food
& Beverage (15,543,469.7, 40.1%) and Electronics (8,608,962.8, 22.2%).

### Translation

Translate you text using `translate_narrative()` function, specify
`language` argument in English:

``` r
translation <- translate_narrative(narrative_enhanced, language = "Czech")
cat(translation)
```

Celková prodejní částka pro všechny regiony dosahuje impresivní částky
38 790 478,4. Regiony, které vynikaly v oblasti prodeje, byly Severní
Amerika (18 079 736,4, 46,6 %) a EMEA (13 555 412,7, 34,9 %). V Severní
Americe produkty, které výrazně přispěly k prodeji, byly Food & Beverage
(7 392 821, 40,9 %) a Elektronika (3 789 132,7, 21 %). Podobně v regionu
EMEA hrály v prodejním růstu významnou roli Food & Beverage (5 265
113,2, 38,8 %) a Elektronika (3 182 803,4, 23,5 %). Produkty, které se
mimořádně osvědčily napříč všemi regiony, byly Food & Beverage (15 543
469,7, 40,1 %) a Elektronika (8 608 962,8, 22,2 %).

### Summarization

If your output is too verbose you can summarize it with
`summarize_narrative()` function:

``` r
summarization <- summarize_narrative(narrative_enhanced)
cat(summarization)
```

Total sales were 38.7M, with North America and EMEA leading with 18.1M
and 13.5M in sales, respectively. In both regions, Food & Beverage and
Electronics were top-selling products. Overall best-performing products
were Food & Beverage and Electronics.
