library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Virtues Breakdown"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Inner", solidHeader = TRUE,
        collapsible = TRUE,
        valueBoxOutput("inner")),
      box(
        title = "Outer", solidHeader = TRUE,
        collapsible = TRUE,
        valueBoxOutput("outer")),
      box(
        title = "Regulatory", solidHeader = TRUE,
        collapsible = TRUE,
        valueBoxOutput("regulatory"))
    ),
    
    fluidRow(
      box(
        title = "Controls",
        subtitle = "a",
        sliderInput("slider", "Item number:", 1, 1000, 50)
      )
    )
  )
)

# inner Courage Wisdom
inner_list <-c(
"Creativity",
"Curiosity",
"LoveOfLearning",
"Perspective",
"Bravery",
"Persistence",
"Integrity",
"Zest"
)

# outer Humanity Justice
outer_list <-c(
"Love",
"Kindness",
"SocialIntelligence",
"Fairness",
"Leadership"
)

# regulatory Transcendence, Temperance
regulatory_list <-c(
"Forgiveness",
"Humility",
"Prudence",
"Self-Regulation",
"Appreciation",
"Gratitude",
"Hope",
"Humor",
"Spirituality"
)

percentile<-function( x, v ){
  floor(pnorm( x, mean=mean(v), sd=sd(v))*100.)
}

server <- function(input, output) {
  virtues_data <- dfp

  inner_vals <- rowMeans(virtues_data[, inner_list])
  outer_vals <- rowMeans(virtues_data[, outer_list])
  regulatory_vals <- rowMeans(virtues_data[, regulatory_list])

  
  inner_perc <- function(j){ 
    percentile( inner_vals[j], inner_vals )
  }
  outer_perc <- function(j){
    percentile( outer_vals[j], outer_vals)
  }
  regulatory_perc <- function(j) {
    percentile( regulatory_vals[j], regulatory_vals)
  }
  
  output$inner <- renderValueBox({
    valueBox(
      paste0(inner_perc(input$slider), "%"), 
      subtitle = "Inner Virtues",
      color = "orange"
    )
  })
  
  output$outer <- renderValueBox({
    valueBox(
      paste0(outer_perc(input$slider), "%"), 
      subtitle = "Outer Virtues", 
      color = "aqua"
    )
  })

  output$regulatory <- renderValueBox({
    valueBox(
      paste0(regulatory_perc(input$slider), "%"), 
      subtitle = "Regulatory Virtues", 
      color = "green"
    )
  })
}

shinyApp(ui, server)
