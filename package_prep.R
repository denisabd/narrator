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

# website -----------------------------------------------------------------
pkgdown::build_site(lazy = FALSE, new_process = TRUE)

# github actions
# Run once
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")

# devtools checks and test ---------------------------------------------------------
devtools::test()

devtools::check()
rhub::check_for_cran()

# testing -----------------------------------------------------------------
#usethis::use_testthat(3)

library(testthat)

usethis::use_test("narrate_desc")
usethis::use_test("format_number")
usethis::use_test("pluralize")

devtools::test()

# check the whole package
devtools::check()

# code coverage
covr::report()


# licence -----------------------------------------------------------------
usethis::use_mit_license("Denis Abdullin")


# data --------------------------------------------------------------------
usethis::use_data(sales, overwrite = TRUE, internal = TRUE)

library(narrator)
library(dplyr)

sales %>%
  narrate_descriptive(measure = "Sales",
               dimensions = c("Territory", "State"))

sales <- sales %>%
  select(-year_id, -qtr_id, -month_id, -phone, -addressline1, -addressline2)

names(sales) <- stringr::str_to_title(names(sales))
sales

# vignettes ---------------------------------------------------------------
usethis::use_vignette(name = "intro", title = "Introducing narrator")


# CRAN --------------------------------------------------------------------
usethis::use_cran_badge()
