
library(shinyjs)

#source("create_data.R")

## This is a A

metricVecA <- c("Percent High School Diploma" = "perchsd",
               "Percent College" = "percollege", 
               "Percent Professional Degree" = "percprof",
               "Percent Below Poverty" = "percbelowpoverty", 
               "Percent Children Below Poverty" = "percbelowpoverty", 
               "Percent Elderly Poverty" = "percelderlypoverty")

#shinyApp(uiA, serverA)

uiA <- fluidPage(
  useShinyjs(),
  
  titlePanel("Shiny Project -- A"),
  
  fluidRow(column(4, wellPanel(selectInput("state", "Select State", choices = unique(mapData$State), selected = "Ohio"),
                               actionButton("createMap", "Update State"), HTML("&nbsp;&nbsp;&nbsp;"),
                               actionButton("clearButton", "Restore Defaults")))),
  hr(),
  column(12, 
         tabsetPanel(
           tabPanel("Choropleth", 
                    fluidRow(column(4, wellPanel(
                      selectInput("metric", "Select Metric", choices = metricVecA), 
                      selectInput("cscale", "Which color scale:", choices = c("Gradient", "Viridis"), selected = "Gradient"),
                      uiOutput("scale_val"), 
                      uiOutput("scale_val2")),
                    ), column(7, div(id = "test", plotOutput("choropleth"))))),  
           
           
           tabPanel("College List", fluidRow(column(6, DT::dataTableOutput("collegeList"), offset = 2))),
           tabPanel("College Analysis", 
                    fluidRow(column(3, selectInput("xv", "x Axis", c("Enrollment" = "Enrollment", 
                                                                     "Cost" = "Cost")), offset = 3),
                             column(3, selectInput("yv", "y Axis", c("Percent Female" = "PercentFemale",
                                                                     "Average Faculty Salary" = "Avg_Fac_Salary")))), 
                    fluidRow(column(6, plotOutput("otherplot"), offset = 3))))) 
)

serverA <- function(input, output) {
  
  observeEvent(input$clearButton, {
    reset("createMap")
    reset("xv")
    reset("yv")
    reset("metric")
    reset("state")
    reset("gval1")
    reset("gval2")
    reset("test")
    reset("cscale")
    
  })
  
  output$otherplot <- renderPlot({
    input$createMap
    tt <- subset(colleges, STATE == isolate(input$state))
    p <- ggplot(tt, aes_string(input$xv, input$yv)) + geom_point() + theme_bw()
    if (input$yv == "PercentFemale") {
      p + labs(x = input$xv, y = "Percent Female", title = paste0("Colleges in ", isolate(input$state)))
    } else {
      p + labs(x = input$xv, y = "Average Faculty Salary", title = paste0("Colleges in ", isolate(input$state)))
    }
  })
  
  mapdat <- eventReactive(input$createMap, {subset(mapData, State == input$state)})
  statedat <- eventReactive(input$createMap, {subset(state_mid, region == input$state)})
  
  output$collegeList <- DT::renderDataTable({
    #req(mapdat()); 
    input$createMap
    subset(colleges, STATE == isolate(input$state))[, c(1:8, 12)]
  }, options = list(pageLength = 5, columnDefs = list(list(className = 'dt-center', targets = 4))))
  
  output$choropleth <- renderPlot({
    req(mapdat())
    p <- ggplot() + 
      geom_polygon(data = mapdat(), aes_string(x = "long", y = "lat", group = "group",
                                               fill = input$metric),
                   color = "black", linewidth = 0.2, alpha = 0.75) +
      geom_polygon(data = statedat(), aes(x = long, y = lat, group = group, fill = NULL), 
                   fill = NA, color = "black", linewidth = 0.5) + theme_bw() +
      labs(title = paste0("State Choropleth: ", isolate(input$state)), xlab = "Longitude", ylab = "Latitude")
    if (input$cscale == "Viridis") {
      p + scale_fill_continuous(type = "viridis", option = input$vval) + labs(fill = "Metric")
    } else {
      p + scale_fill_gradient(low = str_to_lower(input$gval1), high = str_to_lower(input$gval2)) + labs(fill = "Metric")
    }
    
  }, res = 96, width = 450, height = 450)
  
  output$scale_val <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval1", "Set color one", setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), isolate(input$gval2)), selected = "Red")
    } else {
      selectInput("vval", "Set scale", c("Magma" = "A", "Viridis" = "D", "Cividis" = "E", "Turbo" = "H"), selected = "H")
    }
  })
  output$scale_val2 <- renderUI({
    if (input$cscale == "Gradient") {
      selectInput("gval2", "Set color two", setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), isolate(input$gval1)))
    } else {
      NULL
    }
  })
  
  observeEvent(input$gval2, {
    curval <- input$gval1
    if (input$gval2 == input$gval1) {
      updateSelectInput(inputId = "gval1", 
                        choices = setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval2))
    } else {
      updateSelectInput(inputId = "gval1", 
                        choices = setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval2), selected = curval)
    }
  })
  
  observeEvent(input$gval1, {
    curval <- input$gval2
    if (input$gval2 == input$gval1) {
      updateSelectInput(inputId = "gval2", 
                        choices = setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval1))
    } else {
      updateSelectInput(inputId = "gval2", 
                        choices = setdiff(c("Green", "Red", "Blue", "Purple", "Yellow"), input$gval1), selected = curval)
    }
  })
  
}


#shinyApp(uiA, serverA)



























