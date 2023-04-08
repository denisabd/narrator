
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

Package is available in both R and Python, with all core features and
even syntax being the same or similar. Corresponding classes and data
types are used in both languages:

- data.frame vs pandas dataframe
- list vs dictionary
- character vector vs list

## Installation

For Python install `pynarrator` from pip:

``` bash
pip3 install pynarrator
```

For R you can install the development version of narrator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("denisabd/narrator")
```

## Basic Use Cases

Simple tables with one or more categorical columns (dimensions) and one
measure can be transformed to text using `narrate_descriptive()`
function.

### R

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

### Python

``` python
import os
from pynarrator import narrate_descriptive, read_data
import pprint
```

``` python
sales = read_data()
```

``` python
narrative_one = narrate_descriptive(
  df = sales, 
  measure = 'Sales', 
  dimensions = ['Region', 'Product'], 
  return_data = False, 
  coverage = 0.5
  )
  
pprint.pprint(narrative_one)
#> {'Product by Sales': 'Outlying Products by Sales are Food & Beverage '
#>                      '(15543469.7, 40.0%), Electronics (8608962.8, 22.0%).',
#>  'Region by Sales': 'Outlying Regions by Sales are NA (18079736.4, 47.0%), '
#>                     'EMEA (13555412.7, 35.0%).',
#>  'Total Sales': 'Total Sales across all Regions is 38790478.42.'}
```

<!-- You can analyze changes over time using `narrate_trend()` function: -->
<!-- ```{r} -->
<!-- narrative_two <- sales %>% -->
<!--   narrate_trend( -->
<!--     measure = "Sales", -->
<!--     date = "Date", -->
<!--     dimensions = c("Region", "Product") -->
<!--   ) -->
<!-- narrative_two -->
<!-- ``` -->

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

The total amount of sales across all global regions stands at
38,790,478.4 units. Notably, North America (NA) and Europe, Middle-East,
and Africa (EMEA) lead the sales charts with 18,079,736.4 units (46.6%)
and 13,555,412.7 units (34.9%) respectively.

In the North American region, the significant products driving sales are
Food & Beverage (7,392,821 units, 40.9%) and Electronics (3,789,132.7
units, 21%). Whereas, in EMEA, Food & Beverage (5,265,113.2 units,
38.8%) and Electronics (3,182,803.4 units, 23.5%) dominate the sales
figures.

Moreover, when exploring product-specific sales figures, we see that
Food & Beverage (15,543,469.7 units, 40.1%) and Electronics (8,608,962.8
units, 22.2%) remain the outlying products in terms of sales figures.

Similar in Python

``` python
narrative_enhanced = enhance_narrative(narrative_one)
```

### Translation

Translate you text using `translate_narrative()` function, specify
`language` argument in English:

``` r
translation <- translate_narrative(narrative_enhanced, language = "Czech")
cat(translation)
```

Celkový počet prodaných výrobků všech globálních oblastí činí 38 790
478,4 jednotek. Zvláště severní Amerika (NA) a Evropa, Střední východ a
Afrika (EMEA) vedou prodejní statistiky s 18 079 736,4 jednotkami (46,6
%) a 13 555 412,7 jednotkami (34,9 %) odpovídajícě.

V oblasti Severní Ameriky jsou hlavními výrobky, které stimulují prodej,
potraviny a nápoje (7 392 821 jednotek, 40,9 %) a elektronika (3 789
132,7 jednotek, 21 %). Na druhé straně, v oblasti EMEA dominují
potraviny a nápoje (5 265 113,2 jednotek, 38,8 %) a elektronika (3 182
803,4 jednotek, 23,5 %).

Navíc, při zkoumání prodeje konkrétních výrobků, vidíme, že potraviny a
nápoje (15 543 469,7 jednotek, 40,1 %) a elektronika (8 608 962,8
jednotek, 22,2 %) zůstávají výraznými výrobky v termínech prodejních
čísel.

In Python:

``` python
translation = translate_narrative(narrative_enhanced, language = "Czech")
```

### Summarization

If your output is too verbose you can summarize it with
`summarize_narrative()` function:

``` r
summarization <- summarize_narrative(narrative_enhanced)
cat(summarization)
```

Global sales stand at 38,790,478.4 units, with North America and EMEA
leading the charts. In North America, Food & Beverage and Electronics
are significant drivers, while in EMEA, Food & Beverage and Electronics
dominate. Food & Beverage and Electronics continue to be the outlying
products in terms of sales figures.

In Python:

``` python
summarization = summarize_narrative(narrative_enhanced)
```
