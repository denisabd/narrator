#' Shiny Module for Narrator
#' @description UI for the shiny module to work together with \link{narratorServer}
#'
#' @param id Namespace specification
#' @param format If `TRUE` - format text with HTML tags
#' @inheritParams narrate_descriptive
#'
#' @importFrom shiny NS fluidRow column selectizeInput sliderInput checkboxInput uiOutput
#'
#' @return UI
#' @export
#'
#' @example inst/shiny/app.R
narratorUI <- function(
    id,
    narration_depth = 2,
    coverage = 0.5,
    coverage_limit = 5,
    format = TRUE,
    ...) {

  ns <- NS(id)
  fluidRow(
    column(width = 3,
           selectizeInput(
             inputId = ns("narration_depth"),
             label = "Narration Depth",
             choices = c(
               "Summary" = 1,
               "Detailed" = 2
             ),
             selected = narration_depth,
             multiple = FALSE
           ),
           sliderInput(
             inputId = ns("coverage"),
             label = "Coverage",
             value = coverage,
             min = 0.01,
             max = 1,
             step = 0.01
           ),
           sliderInput(
             inputId = ns("coverage_limit"),
             label = "Coverage Limit",
             value = coverage_limit,
             min = 1,
             max = 10,
             step = 1
           ),
           checkboxInput(
             inputId = ns("format"),
             label = "Format",
             value = format
             ),

    ),
    column(width = 9,
           uiOutput(ns("narrative_ui"))
    )
  )
}

#' Server Logic for Narrator
#'
#' @description Server logic for the shiny module to work together with \link{narratorUI}
#'
#' @param id Namespace specification
#' @param df Reactive data frame
#' @param summarization Summarization/aggregation for the data - 'sum', 'count' or 'average'
#' @param ... Additional arguments for `narrate_*` functions
#'
#' @importFrom shiny NS moduleServer reactiveValues observeEvent renderUI req
#' @importFrom htmltools HTML
#'
#' @return Narrative output
#' @export
#'
#' @example inst/shiny/app.R
narratorServer <- function(
    id,
    df,
    summarization = "sum",
    ...
) {

  moduleServer(
    id,
    function(input, output, session) {

      # namespace
      ns <- session$ns

      rv <- reactiveValues(
        narrative = NULL
      )

      observeEvent(c(df(), input$narration_depth, input$coverage, input$coverage_limit, input$format), {

        narrative <- df() %>%
          dplyr::ungroup() %>%
          narrate_descriptive(
            summarization = summarization,
            narration_depth = input$narration_depth,
            coverage = input$coverage,
            coverage_limit = input$coverage_limit,
            ...)

        if (input$format == TRUE) {
          narrative <- narrative %>%
            format_pct()

          names(narrative) <- add_tag(names(narrative), "h3")

          narrative <- unlist(narrative)

          narrative <- mapply(c, names(narrative), as.character(narrative)) %>%
            as.character() %>%
            HTML()
        }

        rv$narrative <- narrative
      })

      output$narrative_ui <- renderUI({
        req(rv$narrative)
      })
    }
  )
}
