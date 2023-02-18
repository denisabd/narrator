#' Interactively Edit Package Templates
#'
#'
#' @export
edit_templates <- function() {

  templates <- list_templates() %>%
    dplyr::select(name, type, template)

  # templates %>%
  #   dplyr::mutate(words = stringr::str_extract_all(
  #     template,
  #     "(?<=\\{)[^\\}]+")
  #   )

  df <- narrator::sales %>%
    dplyr::group_by(Region, Product) %>%
    dplyr::summarise(Sales = sum(Sales))

  not.null <- function(x) {
    if (is.null(x)) return(FALSE)
    if (!is.null(x)) return(TRUE)
  }

  narrative_output <- df %>%
    narrate_descriptive(
      return_data = TRUE,
      coverage = 0.4
    )

  ui <- miniUI::miniPage(
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

      DT::DTOutput("template_table"),

      DT::DTOutput("new_table")
    )
  )

  server <- function(input, output, session) {

    data <- shiny::reactiveValues(data = templates)

    shiny::observeEvent(input$edit,{
      shiny::showModal(
        if (length(input$template_table_rows_selected) >=1 ) {

          template_name <- toString(data$data[input$template_table_rows_selected, 1])
          template <- toString(data$data[input$template_table_rows_selected, 3])

          ind <- narrative_output %>%
            purrr::map(template_name) %>%
            purrr::detect_index(not.null)

          shiny::modalDialog(
            size = "l",
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
      data$data[i, 3] <- template_new
      shiny::removeModal()
    })

    shiny::observeEvent(input$export,{
      output <- data$data %>%
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
        data$data,
        rownames = FALSE,
        extensions = "Scroller",
        selection = list(mode = "single"),
        options = list(
          dom = "t",
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE
        ),
        class = 'cell-border stripe'
      )
    })

    output$new_table <- DT::renderDataTable({
      templates_new <- data$data

      do.call(narrator::narrate_descriptive,
              c(list(df = df), split(templates_new$template, templates_new$name))
      ) %>%
        tibble::enframe(name = "Name", value = "Narrative") %>%
        tidyr::unnest(cols = Narrative) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Scroller",
          options = list(
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
