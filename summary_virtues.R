library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Virtues Breakdown"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      valueBox("inner"),
      valueBox("outer"),
      valueBox("regulatory")
    ),
    
    fluidRow(
      box(
        title = "Controls",
        sliderInput("slider", "Item number:", 1, 100, 50)
      )
    )
  )
)

# inner Courage Wisdom
inner_list <-c(
"Creativity",
"Curiosity",
"Openmindedness",
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
"Citizenship",
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
  pnorm( x, mean=mean(v), sd=sd(v))
}

server <- function(input, output) {
  virtues_data <- dfp1

  j <- input$slider
  inner_vals <- RowSums(virtues_data[, inner_list])
  outer_vals <- RowSums(virtues_data[, outer_data])
  regulatory_data <- RowSums(virtues_data[, regulatory_list])

  inner_perc <- percentile( inner_vals[j], inner_vals )
  outer_perc <- percentile( outer_vals[j], outer_vals)
  regulatory_perc <- percentile( regulatory_vals[j], regulatory_vals)
  
  output$inner <- renderValueBox({
    valueBox(
      paste0(inner_perc, "%"), 
      "Inner Virtues",
      color = "orange"
    )
  })
  
  output$inner <- renderValueBox({
    valueBox(
      paste0(outer_perc, "%"), 
      "Inner Virtues", 
      color = "aqua"
    )
  })

  output$inner <- renderValueBox({
    valueBox(
      paste0(regulatory_perc, "%"), 
      "Regulatory Virtues", 
      color = "green"
    )
  })
}

shinyApp(ui, server)
