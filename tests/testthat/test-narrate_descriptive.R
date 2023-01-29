test_that("narrate_descriptive() works", {
  expect_no_error(sales %>%
                    narrate_descriptive(measure = "Sales",
                                        dimensions = c("Territory", "State")))
})

test_that("narrate_descriptive() returns character", {
  expect_type(
    sales %>%
      narrate_descriptive(measure = "Sales",
                          dimensions = c("Territory", "State")),
    "character")
})

test_that("narrate_descriptive() returns an error when no numeric column is available", {
  expect_error(
    sales %>%
      select(Orderdate, Status, State) %>%
      distinct() %>%
      narrate_descriptive()
  )
})

test_that("narrate_descriptive() returns an error when df isn't a data.frame", {
  expect_error(
    list(1, 2, 3) %>%
      narrate_descriptive()
  )
})

test_that("narrate_descriptive() returns an error when no dimension columns are available", {
  expect_error(
    sales %>%
      select(Orderdate, Sales) %>%
      distinct() %>%
      narrate_descriptive()
  )
})
