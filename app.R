library(shiny)
library(DT)
library(bupaR)
library(shinycssloaders)
library(processanimateR)
library(processmapR)
library(xesreadR)
library(tidyverse)

options(shiny.maxRequestSize=1*1024^3) # upload limit = 1GB

### constraints

responded_existence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(xcount = sum(activity_id == activity1),
              ycount = sum(activity_id == activity2)) %>%
    mutate(resp = xcount == 0 | ycount > 0) %>%
    select(CASE_concept_name, resp)
}

response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>% 
    mutate(xoccurs = activity1 %in% activity_id) %>%
    filter(activity_id == activity1 | activity_id == activity2 | !xoccurs) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs)) %>%
    mutate(resp = last_activity == activity2 | !xoccurs)  %>%
    select(CASE_concept_name, resp)
}


precedence <- function(eventlog, activity1, activity2) {
  
  eventlog %>%
    group_by(CASE_concept_name) %>% 
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 | activity_id == activity2 | !yoccurs) %>%
    summarize(first_activity = first(activity_id),
              yoccurs = first(yoccurs)) %>%
    mutate(resp = first_activity == activity1 | !yoccurs) %>%
    select(CASE_concept_name, resp)
}


chain_response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(next.activity = lead(activity_id),
           response = activity_id == activity1 & 
                      (is.na(next.activity) | next.activity != activity2)) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}


chain_precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(previous.activity = lag(activity_id),
           response = activity_id == activity2 & 
                      (is.na(previous.activity) | previous.activity != activity1)) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}


first_constraint <- function(eventlog, activity1) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = first(activity_id) == activity1) %>%
    select(CASE_concept_name, resp)
}

last_constraint <- function(eventlog, activity1) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = last(activity_id) == activity1) %>%
    select(CASE_concept_name, resp)
}


constraints = c("Responded Existence",
                "Response",
                "Precedence",
                "Chain Response",
                "Chain Precedence",
                "First",
                "Last")

dual_constraints = c("Responded Existence",
                     "Response",
                     "Precedence",
                     "Chain Response",
                     "Chain Precedence")

### Application

ui <- fluidPage(
  titlePanel("UI"),
  sidebarPanel(
    fluidRow(
      fileInput("xes_input", "Choose XES File",
                accept = c("text/xes", ".xes"))
    ),
    fluidRow(
      sliderInput(
        "data_used",
        "% of Data",
        min = 0,
        max = 100,
        post = "%",
        value = 90
      )
    ),
    fluidRow(
      selectInput("x", "None", label = "Activity A")
      ),
    uiOutput("activity2_input"),
    fluidRow(
      selectInput("z", choices = constraints, label = "Constraint")
      ),
    fluidRow(
      actionButton(
        inputId = "act",
        label = "Add Constraint",
        icon("plus", lib = "glyphicon")
      )
    ),
    fluidRow(DT::dataTableOutput("table"))
  ),
  mainPanel("Output",
            fluidPage(
              width = 12,
              shinycssloaders::withSpinner(processanimaterOutput("process", height="800px"))
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
                                    
      fluidRow(
        selectInput("y", possible_activities, label = "Activity B")
      )
    }
  })
  
  # read file only if fileinput changes
  observeEvent(input$xes_input, {
    RV$eventlog <- read_xes(input$xes_input$datapath)
    RV$filters <- data.frame(row.names = unique(RV$eventlog$CASE_concept_name))
    RV$constraints <- NULL
    
    # set content of activity dropdown boxes
    possible_activities <- unique(RV$eventlog$activity_id)
    
    updateSelectInput(session = session, inputId = "x", choices=possible_activities)
    updateSelectInput(session = session, inputId = "y", choices=possible_activities)
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
      event_filter$alltrue <- rowSums(event_filter) == length(event_filter) 
      event_filter <- event_filter %>%
        rownames_to_column("case_id") %>%
        filter(!alltrue)
    }
    
    filtered_events <- RV$eventlog %>% filter(!CASE_concept_name %in% event_filter$case_id)
    
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
    constraint = switch(input$z, 
                        "Responded Existence" = responded_existence,
                        "Response" = response,
                        "Precedence" = precedence,
                        "Chain Response" = chain_response,
                        "Chain Precedence" = chain_precedence,
                        "First" = first_constraint,
                        "Last" = last_constraint)
    
    if (input$z %in% dual_constraints) {
      constraint_matches = constraint(RV$eventlog, input$x, input$y)
    } else {
      constraint_matches = constraint(RV$eventlog, input$x)
    }

    # dirty hacks to merge new constraints with old constraints
    constraint_matches <- constraint_matches %>% 
      column_to_rownames("CASE_concept_name")
    colnames(constraint_matches) <- toString(counter$countervalue)
    RV$filters = merge(RV$filters, constraint_matches, by="row.names") %>% 
      remove_rownames %>% 
      column_to_rownames(var="Row.names")
    
    # create a new entry
    newrow = data.frame(
      id = counter$countervalue,
      activity1 = input$x,
      activity2 = input$y,
      constraint = input$z,
      filtered = paste0(round((1 - sum(constraint_matches[[1]]) / length(constraint_matches[[1]])) * 100, 1), "%"),
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
      pageLength = 10,
      dom = 't',
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
    RV$constraints <<- RV$constraints[-which(RV$constraints$id == selectedId), ]
  })
  
  observeEvent(input$sliderData, {
    print(input$sliderData)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

#runApp()