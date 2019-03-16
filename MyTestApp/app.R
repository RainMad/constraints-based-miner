library(shiny)
library(shinydashboard)
library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(petrinetR)
ui <- fluidPage(
  dashboardHeader(),
  dashboardSidebar(
    
    selectInput("resources","Select the resource", c("r1","r2","r3","r4","r5"),selected = "r1",selectize = T)
  ),
  dashboardBody(
    uiOutput("ui")
  ))
server <- function(input, output) { 
  output$ui <- renderUI({
    r <- input$resources
    tagList(patients %>% process_map())
  })
}
shinyApp(ui, server)