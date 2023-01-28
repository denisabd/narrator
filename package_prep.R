#install.packages("usethis")
#install.packages("devtools")
#usethis::create_package("C:/projects/narrator")


usethis::use_package("glue")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("lubridate")
usethis::use_package("pluralize")
usethis::use_package("readr")

usethis::use_pipe()

# build package -----------------------------------------------------------
devtools::document()
devtools::install(upgrade = "never", build_vignettes = TRUE)

# devtools checks ---------------------------------------------------------
devtools::check()
rhub::check_for_cran()


# testing -----------------------------------------------------------------
#usethis::use_testthat(3)

library(testthat)
testthat::test_check(package = "narrator")

usethis::use_test("narrate_desc")

devtools::test()
devtools::check()

# licence -----------------------------------------------------------------
usethis::use_mit_license("Denis Abdullin")


# data --------------------------------------------------------------------
usethis::use_data(sales)

library(narrator)

sales %>%
  narrate_desc(measure = "sales",
               dimensions = c("territory", "state"))

sales <- read.csv("sales_data_sample.csv", na = "") %>%
  readr::type_convert() %>%
  as_tibble()

names(sales) <- tolower(names(sales))

sales$orderdate <- as.Date(sales$orderdate,format = "%m/%d/%Y")
sales$orderdate

sales

# vignettes ---------------------------------------------------------------
usethis::use_vignette(name = "intro", title = "Introducing narrator")

# website -----------------------------------------------------------------
pkgdown::build_site(lazy = FALSE, new_process = TRUE)

# github actions
# Run once
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")


# CRAN --------------------------------------------------------------------
usethis::use_cran_badge()
