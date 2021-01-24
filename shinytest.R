library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Virtues Breakdown"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("inner"))
  ))

server <- function(input, output) {
  inner_perc <- 95
  
  output$inner <- renderValueBox({
    valueBox(
      paste(inner_perc, "%"), 
      subtitle = "Inner Virtues",
      color = "orange"
    )
  })
}

shinyApp(ui, server)
