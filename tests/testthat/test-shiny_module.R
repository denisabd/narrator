test_that("narratorUI works", {
  expect_s3_class(narratorUI("main"), "shiny.tag")
})

test_that("narratorServer works", {
  shiny::testServer(
    narratorServer,
    args = list(
      df =  sales %>%
        dplyr::group_by(Region) %>%
        dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
        shiny::reactive()
    ), {
      session$setInputs(
        coverage = 0.5,
        coverage_limit = 3,
        narration_depth = 2,
        format = TRUE
      )

      expect_s3_class(rv$narrative, "html")
    })
})

test_that("narratorServer works", {
  shiny::testServer(
    narratorServer,
    args = list(
      df =  sales %>%
        dplyr::group_by(Region) %>%
        dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
        shiny::reactive()
    ), {
      session$setInputs(
        coverage = 0.5,
        coverage_limit = 3,
        narration_depth = 2,
        format = FALSE
      )

      expect_type(rv$narrative, "list")
    })
})

test_that("narratorServer return correct narrative", {
  shiny::testServer(
    narratorServer,
    args = list(
      df =  sales %>%
        dplyr::group_by(Region) %>%
        dplyr::summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
        shiny::reactive()
    ), {
      session$setInputs(
        coverage = 0.5,
        coverage_limit = 3,
        narration_depth = 2,
        format = FALSE
      )

    expect_equal(
      rv$narrative,
      narrate_descriptive(df = df(), coverage = 0.5, coverage_limit = 3, narration_depth = 2)
    )
  })
})

