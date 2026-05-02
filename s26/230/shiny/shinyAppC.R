
library(shiny)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(DT)



#source("create_data.R")

## This is a C
uiC <- fluidPage(
  
  titlePanel("Shiny Project -- C"),
  
  fluidRow(column(4, wellPanel(
    selectInput("state", "Select State", choices = unique(mapData$State), selected = "Ohio"), 
    selectInput("metric", "Select Metric", choices = colnames(mapData)[17:31]), 
    selectInput("cscale", "Which color scale:", choices = c("Gradient", "Viridis")),
    uiOutput("scale_val"), 
    uiOutput("scale_val2"), 
    actionButton("createMap", "Create Map")    
  )), column(8, 
             mainPanel(plotOutput("choropleth"))
  )), 
)

serverC <- function(input, output) {

  #mapdat <- eventReactive(input$createMap, {subset(mapData, State == input$state)})
  #statedat <- eventReactive(input$createMap, {subset(state_mid, region == input$state)})

  mapdat <- reactive( {subset(mapData, State == input$state)})
  statedat <- reactive( {subset(state_mid, region == input$state)})
  

  output$choropleth <- renderPlot({
    #req(mapdat())
    
    if (input$metric == "percasian") input$metric + 5 # has an error
    
    p <- ggplot() + 
      geom_polygon(data = mapdat(), aes_string(x = "long", y = "lat", group = "group",
                                               fill = input$metric),
                   color = "black", linewidth = 0.2, alpha = 0.75) +
      geom_polygon(data = statedat(), aes(x = long, y = lat, group = group, fill = NULL), 
                   fill = NA, color = "black", linewidth = 0.5)
    if (input$cscale == "Viridis") {
      p + scale_fill_continuous(type = "viridis", option = input$vval) + labs(fill = "Metric")
    } else {
      p + scale_fill_gradient(low = str_to_lower(input$gval1), high = str_to_lower(input$gval2)) + labs(fill = "Metric")
    }
    
  })
  
  output$scale_val <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval1", "Set color one", str_to_title(c("green", "red", "blue")), selected = "Green")
    } else {
      selectInput("vval", "Set scale", LETTERS[1:8], selected = "H")
    }
  })
  output$scale_val2 <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval2", "Set color two", str_to_title(c("green", "red", "blue")), selected = "Red")
    } else {
      NULL
    }
  })
}

#shinyApp(uiC, serverC)




