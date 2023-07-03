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
devtools::build_readme()
pkgdown::build_site(lazy = FALSE, new_process = TRUE)

# github actions
# Run once
#usethis::use_github_action("pkgdown")
#usethis::use_github_action("test-coverage")
#usethis::use_github_action_check_standard()
#usethis::use_lifecycle_badge(stage = "experimental")

# devtools checks and test ---------------------------------------------------------
devtools::test()
devtools::check()

# code coverage
covr::report()

# release -----------------------------------------------------------------
usethis::use_release_issue()

devtools::build_readme()
urlchecker::url_check()
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_win_devel()
rhub::check_for_cran()

# CRAN SUBMISSION
#usethis::use_version('minor')
#devtools::submit_cran()

# licence -----------------------------------------------------------------
usethis::use_mit_license("Denis Abdullin")

# vignettes ---------------------------------------------------------------
usethis::use_vignette(name = "formatting", title = "Formatting Output")
usethis::use_vignette(name = "echarts", title = "Narrate Echarts Plots")
usethis::use_vignette(name = "templates", title = "Editing Templates")
usethis::use_vignette(name = "trend", title = "Trend Narratives")


# CRAN --------------------------------------------------------------------
usethis::use_cran_badge()
