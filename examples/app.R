library(shiny)
library(narrator)
library(dplyr)

df <- sales %>%
  group_by(Region, Product) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE))

ui <- fluidPage(
  narratorUI(id = "main")
)

server <- function(input, output, session) {
  narratorServer(
    id = "main",
    df = reactive(df),
    format = TRUE,
    summarization = "sum")
}

if (interactive()) shinyApp(ui, server)
