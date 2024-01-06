
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator <img src="man/figures/hex.png" align="right" width="160"/>

<!-- badges: start -->

\[![CRAN status](https://www.r-pkg.org/badges/version/narrator)
[![Codecov test
coverage](https://codecov.io/gh/denisabd/narrator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/denisabd/narrator?branch=main)
[![R-CMD-check](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/denisabd/narrator/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

Template-based NLG framework for creating text narratives out of data
and enhance them using ChatGPT. Demo [shiny
application](https://deny.shinyapps.io/narrator_app/) showing core
package capabilities is deployed on shinyapps.io.

Package is available in both R and Python, with all core features and
even syntax being the same or similar. Corresponding classes and data
types are used in both languages:

- data.frame vs pandas data frame
- list vs dictionary
- character vector vs list

# Installation

For R you can install the development version of narrator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("denisabd/narrator")
```

For Python install `pynarrator` from pip:

``` bash
pip3 install pynarrator
```

# R

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
#> Outlying Regions by Sales are NA (18079736.4, 46.6 %) and EMEA (13555412.7, 34.9 %).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales are Food & Beverage (7392821, 40.9 %) and Electronics (3789132.7, 21 %).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales are Food & Beverage (5265113.2, 38.8 %) and Electronics (3182803.4, 23.5 %).
#> 
#> $`Product by Sales`
#> Outlying Products by Sales are Food & Beverage (15543469.7, 40.1 %) and Electronics (8608962.8, 22.2 %).
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
#> Regions with biggest changes of Sales are NA (533.1 K, 9.1 %, 5.9 M to 6.4 M) and EMEA (416.9 K, 9.91 %, 4.2 M to 4.6 M).
#> 
#> $`NA by Product`
#> In NA, significant Products by Sales change are Food & Beverage (243.3 K, 9.92 %, 2.5 M to 2.7 M) and Tools (190.5 K, 32.72 %, 582.2 K to 772.7 K).
#> 
#> $`EMEA by Product`
#> In EMEA, significant Products by Sales change are Electronics (313.1 K, 36.05 %, 868.6 K to 1.2 M) and Food & Beverage (244.8 K, 15.01 %, 1.6 M to 1.9 M).
#> 
#> $`Sales change by Product`
#> Products with biggest changes of Sales are Food & Beverage (535.4 K, 10.63 %, 5 M to 5.6 M) and Electronics (525.9 K, 19.79 %, 2.7 M to 3.2 M).
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

Total Sales across all Regions amount to \$38,790,478.4. The Outlying
Regions, namely North America (NA) and Europe, the Middle East, and
Africa (EMEA), account for significant portions of these sales. NA
contributes \$18,079,736.4, representing 46.6% of the total sales, while
EMEA contributes \$13,555,412.7, representing 34.9%.

Within the NA region, the top-performing product categories by sales are
Food & Beverage, generating \$7,392,821 (40.9%), and Electronics,
generating \$3,789,132.7 (21%). In the EMEA region, the significant
product categories by sales are Food & Beverage, generating
\$5,265,113.2 (38.8%), and Electronics, generating \$3,182,803.4
(23.5%).

When considering all regions, the outlying products that contribute
significantly to sales are Food & Beverage, generating \$15,543,469.7
(40.1%), and Electronics, generating \$8,608,962.8 (22.2%).

### Translation

Translate you text using `translate_narrative()` function, specify
`language` argument in English:

``` r
translation <- translate_narrative(narrative_enhanced, language = "Czech")
cat(translation)
```

Celkové tržby ve všech regionech činí 38 790 478,4 dolarů. Okrajové
regiony, konkrétně Severní Amerika (NA) a Evropa, Střední východ a
Afrika (EMEA), představují významnou část těchto tržeb. NA přispívá
částkou 18 079 736,4 dolarů, což představuje 46,6% celkových tržeb,
zatímco EMEA přispívá částkou 13 555 412,7 dolarů, což představuje
34,9%.

V rámci regionu NA jsou nejúspěšnějšími kategoriemi produktů podle tržeb
potraviny a nápoje, které generují 7 392 821 dolarů (40,9%), a
elektronika, která generuje 3 789 132,7 dolarů (21%). V regionu EMEA
jsou významné kategorie produktů podle tržeb potraviny a nápoje, které
generují 5 265 113,2 dolarů (38,8%), a elektronika, která generuje 3 182
803,4 dolarů (23,5%).

Při zohlednění všech regionů jsou okrajové produkty, které významně
přispívají k tržbám, potraviny a nápoje, které generují 15 543 469,7
dolarů (40,1%), a elektronika, která generuje 8 608 962,8 dolarů
(22,2%).

### Summarization

If your output is too verbose you can summarize it with
`summarize_narrative()` function:

``` r
summarization <- summarize_narrative(narrative_enhanced)
cat(summarization)
```

Total Sales: \$38.8M with North America and EMEA leading. NA contributes
\$18.1M (46.6%) and EMEA contributes \$13.6M (34.9%). Top product
categories in NA are Food & Beverage (\$7.4M) and Electronics (\$3.8M).
In EMEA, it’s Food & Beverage (\$5.3M) and Electronics (\$3.2M).
Overall, Food & Beverage generates \$15.5M (40.1%) and Electronics
generates \$8.6M (22.2%).

# Python

Here are some basic Python examples, for more details visit [pynarrator
github](https://github.com/denisabd/pynarrator) and [pynarrator
website](https://denisabd.github.io/pynarrator/)

``` python
import os
from pynarrator import narrate_descriptive, read_data, enhance_narrative, translate_narrative, summarize_narrative
```

``` python
sales = read_data()
```

By default `narrate_descriptive()` returns a dictionary of narratives
with names.

``` python
narrate_descriptive(
  df = sales, 
  measure = 'Sales', 
  dimensions = ['Region', 'Product'], 
  return_data = False, 
  coverage = 0.5
  )
```

When `simplify = True` the output is a list:

``` python
narrate_descriptive(
  df = sales, 
  measure = 'Sales', 
  dimensions = 'Region', 
  return_data = False, 
  simplify = True,
  coverage = 0.5
  )
```

``` python
narrative_two = narrate_descriptive(
  df = sales, 
  measure = 'Sales', 
  dimensions = 'Region', 
  return_data = False, 
  simplify = True,
  coverage = 0.5
  )
  
pprint.pprint(narrative_two)
```

When `return_data=True` we get a list of variables calculated inside of
the function:

``` python
narrate_descriptive(
  df = sales, 
  measure = 'Sales', 
  dimensions = ['Region', 'Product'], 
  return_data = True, 
  simplify = True,
  coverage = 0.5
  )
```

As all other functions, Chat GPT related calls are to similar to those
in R

``` python
narrative_enhanced = enhance_narrative(narrative_one)
translation = translate_narrative(narrative_enhanced, language = "Czech")
summarization = summarize_narrative(narrative_enhanced)
```
