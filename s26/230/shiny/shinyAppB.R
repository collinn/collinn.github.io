
# source("create_data.R")

## This is a B
metricVecB <- c("Percent Highschool Diploma" = "perchsd",
               "Percent College" = "percollege", 
               "Percent Professional  Degree" = "percprof",
               "Percent Below Poverty" = "percbelowpoverty", 
               "Percent Children Below Poverty" = "percbelowpoverty", 
               "Percent Elderly Poverty" = "percelderlypoverty")

#shinyApp(uiB, serverB)

uiB <- fluidPage(
  
  titlePanel("Shiny Project -- B"),
  
  fluidRow(column(4, wellPanel(
    selectInput("state", "Select State", choices = unique(mapData$State), selected = "Ohio"), 
    selectInput("metric", "Select Metric", metricVecB), 
    selectInput("cscale", "Which color scale:", choices = c("Gradient", "Viridis"), selected = "Viridis"),
    uiOutput("scale_val"), 
    uiOutput("scale_val2"), 
    actionButton("createMap", "Create Map")    
  )), column(8, 
             mainPanel(plotOutput("choropleth"))
  )), 
  h3("Colleges in the State:"),
  fluidRow(column(6,DT::dataTableOutput("collegeList"), offset = 2))
)

serverB <- function(input, output) {
  mapdat <- eventReactive(input$createMap, {subset(mapData, State == input$state)})
  statedat <- eventReactive(input$createMap, {subset(state_mid, region == input$state)})
  
  output$collegeList <- DT::renderDataTable({
    req(mapdat()); input$createMap
    subset(colleges, STATE == isolate(input$state))[, c(1:5)]
  }, options = list(pageLength = 5, columnDefs = list(list(className = 'dt-center', targets = 4))))
  
  output$choropleth <- renderPlot({
    req(mapdat())
    p <- ggplot() + 
      geom_polygon(data = mapdat(), aes_string(x = "long", y = "lat", group = "group",
                                               fill = input$metric),
                   color = "black", linewidth = 0.2, alpha = 0.75) +
      geom_polygon(data = statedat(), aes(x = long, y = lat, group = group, fill = NULL), 
                   fill = NA, color = "black", linewidth = 0.5) + theme_bw() +
      labs(title = paste0("State Choropleth ", input$state), xlab = "Longitude", ylab = "Latitude")
    if (input$cscale == "Viridis") {
      p + scale_fill_continuous(type = "viridis", option = input$vval) + labs(fill = "Metric")
    } else {
      p + scale_fill_gradient(low = str_to_lower(input$gval1), high = str_to_lower(input$gval2)) + labs(fill = "Metric")
    }
    
  })
  
  output$scale_val <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval1", "Set color one", setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval2), selected = "Red")
    } else {
      selectInput("vval", "Set scale", c("Magma" = "A", "Viridis" = "D", "Cividis" = "E", "Turbo" = "H"), selected = "H")
    }
  })
  output$scale_val2 <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval2", "Set color two", setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval1))
    } else {
      NULL
    }
  })
}

#shinyApp(uiB, serverB)




