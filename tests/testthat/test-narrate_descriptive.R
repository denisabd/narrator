test_that("narrate_descriptive() works", {
  expect_no_error(sales %>%
                    narrate_descriptive(measure = "Sales",
                                 dimensions = c("Territory", "State")))
})

