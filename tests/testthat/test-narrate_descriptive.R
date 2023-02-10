test_that("narrate_descriptive() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"))
  )
})


test_that("narrate_descriptive() works", {
  expect_no_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        return_data = TRUE)
  )
})

test_that("narrate_descriptive() returns list with return_data = TRUE", {
  expect_type(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        return_data = TRUE),
    "list"
  )
})

test_that("narrate_descriptive() runs with additional arguments", {
  expect_no_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 3,
        use_renviron = TRUE)
  )
})

test_that("narrate_descriptive() throws an error when coverage is 0", {
  expect_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 0)
  )
})

test_that("narrate_descriptive() throws an error when coverage_limit is less than 0", {
  expect_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 0)
  )
})

test_that("narrate_descriptive() throws an error when coverage_limit isn't an integer", {
  expect_error(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        coverage = 0.7,
        coverage_limit = 5.5)
  )
})

test_that("narrate_descriptive() returns a list", {
  expect_type(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product")),
    "list")
})

test_that("narrate_descriptive() returns a character with simplify = TRUE", {
  expect_type(
    narrator::sales %>%
      narrate_descriptive(
        measure = "Sales",
        dimensions = c("Region", "Product"),
        simplify = TRUE),
    "character")
})

test_that("narrate_descriptive() returns an error when no numeric column is available", {
  expect_error(
    narrator::sales %>%
      select(Orderdate, Status, Product) %>%
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
    narrator::sales %>%
      select(Orderdate, Sales) %>%
      distinct() %>%
      narrate_descriptive()
  )
})


test_that("narrate_descriptive() returns an error when no dimension columns are available", {
  expect_equal(
    sales %>%
      dplyr::filter(Product %in% c("Product A", "Product B")) %>%
      dplyr::group_by(Region, Product)  %>%
      dplyr::summarise(Sales = sum(Sales)) %>%
      narrate_descriptive() %>%
      suppressMessages(),
    sales %>%
      dplyr::filter(Product %in% c("Product A", "Product B")) %>%
      dplyr::group_by(Region, Product)  %>%
      dplyr::summarise(Sales = sum(Sales)) %>%
      narrate_descriptive(return_data = TRUE) %>%
      magrittr::extract2(1) %>%
      suppressMessages()
  )
})
