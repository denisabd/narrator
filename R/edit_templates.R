#' Interactively Edit Package Templates
#'
#'
#' @export
edit_templates <- function() {

  templates <- list_templates() %>%
    dplyr::select(name, type, template) %>%
    dplyr::filter(type != "forecast")

  df_descriptive <- narrator::sales %>%
    dplyr::group_by(Region, Product) %>%
    dplyr::summarise(Sales = sum(Sales))

  df_trend <- narrator::sales %>%
    dplyr::mutate(Month = lubridate::floor_date(Date, unit = "months")) %>%
    dplyr::group_by(Region, Product, Month) %>%
    dplyr::summarise(Sales = sum(Sales))

  not.null <- function(x) {
    if (is.null(x)) return(FALSE)
    if (!is.null(x)) return(TRUE)
  }

  narrative_descriptive <- df_descriptive %>%
    narrate_descriptive(
      return_data = TRUE,
      coverage = 0.4
    )

  narrative_trend <- df_trend %>%
    narrate_trend(
      return_data = TRUE,
      coverage = 0.4
    )

  narrative_output <- c(narrative_descriptive, narrative_trend)

  # ui ----------------------------------------------------------------------
  ui <- miniUI::miniPage(
    shiny::tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),

    miniUI::gadgetTitleBar("Edit Templates"),
    miniUI::miniContentPanel(
      ## Your UI items go here.
      shiny::actionButton(
        inputId = "edit",
        label = "Edit",
        class = "btn-info"),

      shiny::actionButton(
        inputId = "export",
        label = "Export",
        class = "btn-warning"),

      shiny::h3("Templates"),
      DT::DTOutput("template_table"),

      shiny::h3("Updated Narrative"),
      DT::DTOutput("new_table")
    )
  )

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      data = templates,
      template_type = NULL
    )

    shiny::observeEvent(input$edit,{
      shiny::showModal(
        if (length(input$template_table_rows_selected) >=1 ) {

          template_name <- toString(rv$data[input$template_table_rows_selected, 1])
          rv$template_type <- toString(rv$data[input$template_table_rows_selected, 2])
          template <- toString(rv$data[input$template_table_rows_selected, 3])

          ind <- narrative_output %>%
            purrr::map(template_name) %>%
            purrr::detect_index(not.null)

          shiny::modalDialog(
            size = "xl",
            shiny::textInput(
              inputId = "single_template",
              label = "Edit Template",
              value = template,
              width = "800px"
            ),

            DT::renderDataTable({
              shiny::req(ind > 0)

              narrative_output[[ind]] %>%
                tibble::enframe(name = "variable") %>%
                dplyr::mutate(value = as.character(value)) %>%
                DT::datatable(
                  rownames = FALSE,
                  height = 800,
                  options = list(
                    pageLength = 10,
                    dom = "t",
                    deferRender = TRUE
                  ),
                  class = 'cell-border stripe'
                )
            }),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("ok", "OK")
            )
          )
        } else {
          shiny::modalDialog(
            title = "Warning",
            paste("Please select the row that you want to edit!" ), easyClose = TRUE
          )
        }
      )
    })

    shiny::observeEvent(input$ok,{
      shiny::req(input$template_table_rows_selected)

      i <- input$template_table_rows_selected
      template_new <- input$single_template
      rv$data[i, 3] <- template_new
      shiny::removeModal()
    })

    shiny::observeEvent(input$export,{
      output <- rv$data %>%
        dplyr::mutate(result = glue::glue("{type}_{name} = {template}

                                          ")) %>%
        dplyr::select(result) %>%
        as.matrix() %>%
        paste(collapse = "")

      shiny::showModal(
        shiny::modalDialog(
          size = "l",
          shiny::textAreaInput(
            inputId = "output",
            label = "Final Templates",
            value = output,
            height = "400px",
            width = "800px"
          )
        )
      )
    })

    output$template_table <- DT::renderDataTable({
      DT::datatable(
        rv$data,
        rownames = FALSE,
        extensions = "Scroller",
        selection = list(mode = "single"),
        options = list(
          pageLength = 10,
          dom = "t",
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE
        ),
        class = 'cell-border stripe'
      )
    })

    output$new_table <- DT::renderDataTable({
      req(rv$template_type)

      templates_descriptive <- rv$data %>%
        dplyr::filter(type == "descriptive")

      templates_trend <- rv$data %>%
        dplyr::filter(type == "trend")

      output_descriptive <- do.call(narrator::narrate_descriptive,
                                    c(list(df = df_descriptive), split(templates_descriptive$template, templates_descriptive$name))
      ) %>%
        tibble::enframe(name = "Name", value = "Narrative") %>%
        tidyr::unnest(cols = Narrative)

      output_trend <- do.call(narrator::narrate_trend,
                        c(list(df = df_trend), split(templates_trend$template, templates_trend$name))
      ) %>%
        tibble::enframe(name = "Name", value = "Narrative") %>%
        tidyr::unnest(cols = Narrative)

      output_descriptive %>%
        dplyr::bind_rows(output_trend) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Scroller",
          options = list(
            pageLength = 10,
            dom = "t",
            deferRender = TRUE,
            scrollY = 200,
            scroller = TRUE
          ),
          class = 'cell-border stripe'
        )
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    shiny::shinyApp(ui, server),
    viewer = shiny::browserViewer()
  )
}
