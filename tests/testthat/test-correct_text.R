test_that("correct_text() works", {
  expect_no_error(
    correct_text("In 2020 total Sales across all Regions (EMEA, NA, ASPAC) is equal to 23.5 M",
                 download_udpipe = FALSE)
    )
})

test_that("correct_text() outputs correct text", {
  expect_equal(
    correct_text("Total Profit across all Regions are 85 M",
                 download_udpipe = FALSE),
    "Total Profit across all Regions is 85 M"
  )
})

test_that("correct_text() outputs correct text", {
  expect_equal(
    correct_text("Total Spend across all Regions are 85 M", measure = "Spend",
                 download_udpipe = FALSE),
    "Total Spend across all Regions is 85 M"
  )
})
