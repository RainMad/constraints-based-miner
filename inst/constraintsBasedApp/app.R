library(shiny)
library(DT)
library(bupaR)
library(shinycssloaders)
library(processanimateR)
library(processmapR)
library(xesreadR)
library(tidyverse)

options(shiny.maxRequestSize = 1 * 1024 ^ 3) # upload limit = 1GB

constraints = c(
  "Responded Existence",
  "Response",
  "Precedence",
  "Chain Response",
  "Chain Precedence",
  "Init",
  "End",
  "Participation",
  "At Most Once"
)

dual_constraints = c("Responded Existence",
                     "Response",
                     "Precedence",
                     "Chain Response",
                     "Chain Precedence")

ui <- fluidPage(
  titlePanel("Constraints Miner"),
  sidebarPanel(
    fluidRow(fileInput(
      "xes_input", "Choose XES File",
      accept = c("text/xes", ".xes")
    )),
    fluidRow(
      selectInput("z", choices = constraints, label = "Constraint"),
      textOutput('constraintDescription', inline = TRUE),
      tags$head(
        tags$style(
          "#constraintDescription{
          display: block;
          color: gray;
          font-size: 15px;
          font-style: italic;
          padding: 0 30px 30px;
          }"
                           )
        )
        ),
    fluidRow(selectInput("x", "None", label = "Activity A")),
    uiOutput("activity2_input"),
    fluidRow(actionButton(
      inputId = "act",
      label = "Add Constraint",
      icon("plus", lib = "glyphicon")
    )),
    fluidRow(DT::dataTableOutput("table"))
        ),
  mainPanel(fluidPage(
    width = 12,
    shinycssloaders::withSpinner(processanimaterOutput("process", height = "800px"))
  ))
)

# Example data
RV <-
  reactiveValues(
    constraints = data.frame(
      id = NULL,
      x = NULL,
      y = NULL,
      xy = NULL,
      Delete = NULL,
      stringsAsFactors = FALSE
    ),
    rowname = NULL,
    eventlog = NULL,
    filters = NULL
  )

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session) {
  # counter value used as id for the releationships
  counter <- reactiveValues(countervalue = 0)
  
  # remove UI for second activity if single constraint is selected
  output$activity2_input <- renderUI({
    if (input$z %in% dual_constraints) {
      if (is.null(RV$eventlog)) {
        possible_activities <- "None"
      } else {
        possible_activities <- unique(RV$eventlog$activity_id)
      }
      
      fluidRow(selectInput("y", possible_activities, label = "Activity B"))
    }
  })
  
  # read file only if fileinput changes
  observeEvent(input$xes_input, {
    RV$eventlog <- read_xes(input$xes_input$datapath)
    RV$filters <-
      data.frame(row.names = unique(RV$eventlog$CASE_concept_name))
    RV$constraints <- NULL
    
    # set content of activity dropdown boxes
    possible_activities <- unique(RV$eventlog$activity_id)
    
    updateSelectInput(session = session,
                      inputId = "x",
                      choices = possible_activities)
    updateSelectInput(session = session,
                      inputId = "y",
                      choices = possible_activities)
  })
  
  # render graph
  output$process <- renderProcessanimater(expr = {
    if (is.null(RV$eventlog)) {
      return()
    }
    
    if (length(RV$filters) == 0) {
      event_filter <- data.frame(case_id = c())
    } else {
      event_filter <- data.frame(RV$filters)
      event_filter$alltrue <-
        rowSums(event_filter) == length(event_filter)
      event_filter <- event_filter %>%
        rownames_to_column("case_id") %>%
        filter(!alltrue)
    }
    
    filtered_events <-
      RV$eventlog %>% filter(!CASE_concept_name %in% event_filter$case_id)
    
    if (count(filtered_events) == 0) {
      return()
    }
    
    animate_process(
      filtered_events,
      mode = "off",
      timeline = TRUE,
      legend = "color",
      initial_state = "paused"
    )
  })
  
  # action which is fired when pressing the Ok Button for inserting constraints
  observeEvent(input$act, {
    # constraints
    constraint = switch(
      input$z,
      "Participation" = participation,
      "At Most Once" = at_most_once,
      "Init" = init_constraint,
      "End" = end_constraint,
      "Responded Existence" = responded_existence,
      "Response" = response,
      # Alternate response
      "Chain Response" = chain_response,
      "Precedence" = precedence,
      # Alternate precedence
      "Chain Precedence" = chain_precedence,
      # Coexistence
      # Succession
      # AlternateSuccession
      # ChainSuccession
      # NotChainSuccession
      # NotSuccession
      # NotCoExistence   
    )
    
    source("constraints.R")
    
    if (input$z %in% dual_constraints) {
      tbl_activity_b <- input$y
      constraint_matches = constraint(RV$eventlog, input$x, input$y)
    } else {
      tbl_activity_b <- "-"
      constraint_matches = constraint(RV$eventlog, input$x)
    }
    
    # dirty hacks to merge new constraints with old constraints
    constraint_matches <- constraint_matches %>%
      column_to_rownames("CASE_concept_name")
    colnames(constraint_matches) <- toString(counter$countervalue)
    RV$filters = merge(RV$filters, constraint_matches, by = "row.names") %>%
      remove_rownames %>%
      column_to_rownames(var = "Row.names")
    
    # create a new entry
    newrow = data.frame(
      Id = counter$countervalue,
      ActivityA = input$x,
      ActivityB = tbl_activity_b,
      Constraint = input$z,
      Filtered = paste0(round((
        1 - sum(constraint_matches[[1]]) / length(constraint_matches[[1]])
      ) * 100, 1), "%"),
      Delete = paste(
        "<button id='button_",
        counter$countervalue,
        "' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>Delete</button>",
        sep = ""
      ),
      stringsAsFactors = FALSE
    )
    
    # increase the id
    counter$countervalue = counter$countervalue + 1
    # add the entry to the table
    RV$constraints <<- rbind(RV$constraints, newrow)
  })
  
  # print the table
  output$table <- DT::renderDataTable(
    RV$constraints,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 5,
      dom = 'tip',
      autoWidth = TRUE,
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      ))
    )
  )
  
  # if the select button of one entry of the table has been pressed, delete the entry
  observeEvent(input$select_button, {
    # get the id of the button which is the same like in the data frame
    selectedId <-
      as.numeric(strsplit(input$select_button, "_")[[1]][2])
    RV$filters[[toString(selectedId)]] <- NULL
    
    # Remove the value of the table
    RV$constraints <<-
      RV$constraints[-which(RV$constraints$Id == selectedId),]
  })
  
  observeEvent(input$z, {
    output$constraintDescription <- renderText({
      switch(
        input$z,
        "Responded Existence" = "If Activity A occurs, then Activity B occurs too",
        "Response" = "If Activity A occurs, then Activity B occurs after A",
        "Precedence" = "B occurs only if preceded by A",
        "Chain Response" = "If Activity A occurs, Activity B occurs immediately after it",
        "Chain Precedence" = "Activity B occurs only if Activity A occurs immediately before it",
        "Init" = "Activity A is the first to occur",
        "End" = "Activity A is the last to occur",
        "Participation" = "Activity A occurs at least once",
        "At Most Once" = "Activity A occurs at most once"
      )
    })
  })
  
  exportTestValues(RV = { RV$eventlog })
}

# Run the application
shinyApp(ui = ui, server = server)

#runApp()