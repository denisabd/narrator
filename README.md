
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Narrator <img src="man/figures/hex.png" align="right" width="160"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/narrator)](https://CRAN.R-project.org/package=narrator)
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

The company’s Total Sales across all Regions has reached an impressive
38790478.4, showcasing its strong market presence. Among the Outlying
Regions, North America (NA) dominates with a total sales value of
18079736.4, accounting for 46.6% of total sales. Additionally, the
Europe, Middle East, and Africa (EMEA) region follows, with a sales
value of 13555412.7, contributing to 34.9% of total sales.

Analyzing the sales data further indicates that in North America,
notable product categories by sales are Food & Beverage, generating a
substantial sales value of 7392821 and accounting for 40.9% of total
sales. Electronics, with sales value of 3789132.7, is also a significant
product category, contributing 21% to total sales. Similarly, in the
EMEA region, Food & Beverage is the top product category, responsible
for generating a sales value of 5265113.2, contributing to 38.8% of
total sales. Electronics come in second, with a sales value of
3182803.4, accounting for 23.5% of sales in the EMEA region.

Overall, when considering outlying products by sales for all regions,
Food & Beverage category remains dominant. It has registered a sales
value of 15543469.7, contributing to 40.1% of total sales. Electronics
come in second among outlying products with a sales value of 8608962.8,
representing 22.2% of total outlying sales.

### Translation

Translate you text using `translate_narrative()` function, specify
`language` argument in English:

``` r
translation <- translate_narrative(narrative_enhanced, language = "Czech")
cat(translation)
```

Celkový objem prodeje společnosti přes všechny regiony dosáhl
impresivního čísla 38790478,4, což ukazuje na její silnou pozici na
trhu. Mezi vzdálenými regiony tvoří Severní Amerika (NA) dominantní
tržbu s celkovou hodnotou prodeje 18079736,4, což odpovídá 46,6%
celkového objemu prodeje. Dále následuje oblast Evropy, Středního
východu a Afriky (EMEA) s hodnotou prodeje 13555412,7, přispívající k
34,9% z celkového prodeje.

Podrobnější analýza prodejních dat ukazuje, že v Severní Americe jsou
významné kategorie produktů podle prodeje potraviny a nápoje, generující
obrovskou tržbu 7392821, což odpovídá 40,9% celkového prodeje.
Elektronika s prodejem ve výši 3789132,7 je také významnou kategorií
produktů, přispívající k 21% celkového prodeje. Obdobně v oblasti EMEA
jsou potraviny a nápoje nejvýznamnější kategorií produktů a jsou
zodpovědné za generování prodeje v hodnotě 5265113,2 což představuje
38,8% z celkového prodeje. Elektronika se umisťuje na druhé pozici s
prodejem ve výši 3182803,4, což odpovídá 23,5% prodeje v oblasti EMEA.

Celkově, pokud se budou posuzovat vzdálené produkty podle prodeje ve
všech oblastech, kategorie potravin a nápojů zůstává dominantní.
Zaregistrovala prodej v hodnotě 15543469,7, přispívající 40,1% k
celkovému prodeji. Elektronika se umisťuje na druhém místě mezi
vzdálenými produkty s prodejem ve výši 8608962,8, což odpovídá 22,2%
celkového prodeje.

### Summarization

If your output is too verbose you can summarize it with
`summarize_narrative()` function:

``` r
summarization <- summarize_narrative(narrative_enhanced)
cat(summarization)
```

Company’s total sales reach 38790478.4. North America leads with 46.6%
of total sales, followed by EMEA with 34.9%. Notable product categories
in NA are Food & Beverage and Electronics, generating 40.9% and 21% of
sales, respectively. In EMEA, Food & Beverage and Electronics account
for 38.8% and 23.5% of sales, respectively. Food & Beverage is the
dominant category across all regions, generating 40.1% of total outlying
sales. Electronics follow with 22.2%.

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
