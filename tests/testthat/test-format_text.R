test_that("format_text works", {
  expect_equal(
    format_text("1.2%", color = "auto", bold = TRUE),
    "<b> <span style='color: green;'>1.2%</span> </b>"
  )
})

test_that("format_text works", {
  expect_equal(
    format_text("-44 M", color = "auto", bold = TRUE),
    "<b> <span style='color: red;'>-44 M</span> </b>"
  )
})

test_that("format_text works", {
  expect_equal(
    format_text("-44 M", color = "orange", bold = FALSE),
    "<span style='color: orange;'>-44 M</span>"
  )
})


