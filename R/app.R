library(shiny)
library(DT)
library(shinycssloaders)
library(processanimateR)
library(xesreadR)
library(tidyverse)

options(shiny.maxRequestSize = 200 * 1024 ^ 2) # upload limit = 200MB (GB 1 * 1024^3)

# All constraints return an eventlog as result which looks like the following
# <Trace1> <Boolean>
# <Trace2> <Boolean>
# <Trace3> <Boolean>
#...
# <TraceN> <Boolean>?
# The boolean indicates if the condition of the constraint is satisfied or not
# For example the constraint 'responded existence' returns TRUE for each 
# trace which contains the acitvities A and B

# if A occurs then B occurs
responded_existence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(
      xcount = sum(activity_id == activity1),
      ycount = sum(activity_id == activity2)
    ) %>%
    mutate(resp = xcount == 0 | ycount > 0) %>%
    select(CASE_concept_name, resp)
}

# if A occurs then B follows
response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id) %>%
    # keep all events which contain activity1 or activity2 and all events of traces which does
    # not contain activity 1
    filter(activity_id == activity1 |
             activity_id == activity2 | !xoccurs) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs)) %>%
    mutate(resp = last_activity == activity2 | !xoccurs)  %>%
    select(CASE_concept_name, resp)
  
  # res <- eventlog %>%
  #   filter_activity_presence(activity1) %>%
  #   filter_precedence(antecedents = activity1, consequents = activity2, precedence_type="eventually_follows")
  # 
  # eventlog %>%
  #   cases %>%
  #   mutate(resp = !(CASE_concept_name %in% res$CASE_concept_name)) %>%
  #   select(CASE_concept_name, resp)
}

# If B occurs then A precedes
precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | !yoccurs) %>%
    summarize(first_activity = first(activity_id),
              yoccurs = first(yoccurs)) %>%
    mutate(resp = first_activity == activity1 | !yoccurs) %>%
    select(CASE_concept_name, resp)
}


chain_response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(
      next.activity = lead(activity_id),
      response = activity_id == activity1 &
        (is.na(next.activity) |
           next.activity != activity2)
    ) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}

chain_precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(
      previous.activity = lag(activity_id),
      response = activity_id == activity2 &
        (is.na(previous.activity) |
           previous.activity != activity1)
    ) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}

init_constraint <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = first(activity_id) == activity) %>%
    select(CASE_concept_name, resp)
}

end_constraint <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = last(activity_id) == activity) %>%
    select(CASE_concept_name, resp)
}

participation <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = activity %in% activity_id) %>%
    select(CASE_concept_name, resp)
}

at_most_once <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(cnt = sum(activity == activity_id)) %>%
    mutate(resp = cnt <= 1) %>%
    select(CASE_concept_name, resp)
}

coexistence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(
      xcount = sum(activity_id == activity1),
      ycount = sum(activity_id == activity2)
    ) %>%
    mutate(resp = ((xcount == 0 & ycount == 0) | (xcount > 0 & ycount > 0))) %>%
    select(CASE_concept_name, resp)
}

succession <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id) %>%
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | (!xoccurs & !yoccurs)) %>%
  summarize(last_activity = last(activity_id),
            xoccurs = first(xoccurs),
            first_activity = first(activity_id),
            yoccurs = last(yoccurs)) %>%
  mutate(resp = last_activity == activity2 & first_activity == activity1 | (!xoccurs & !yoccurs))  %>%
    select(CASE_concept_name, resp)
}

chain_succession <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id,
           yoccurs = activity2 %in% activity_id,
           previous.activity = lag(activity_id),
           next.activity = lead(activity_id),
           chain = (activity_id != activity1 & activity_id != activity2) |
             (xoccurs & !is.na(next.activity) & next.activity == activity2) | 
             (yoccurs & !is.na(previous.activity) & previous.activity == activity1)) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | (!xoccurs & !yoccurs)) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs),
              first_activity = first(activity_id),
              yoccurs = last(yoccurs),
              chain = all(chain == TRUE)) %>%
    mutate(resp = last_activity == activity2 &
                  first_activity == activity1 &
                  chain | 
                  (!xoccurs & !yoccurs))  %>%
    select(CASE_concept_name, resp)
}

not_chain_succession <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id,
           yoccurs = activity2 %in% activity_id,
           previous.activity = lag(activity_id),
           next.activity = lead(activity_id),
           chain = ((activity_id != activity1 & activity_id != activity2) |
                      activity_id == activity2 & (is.na(previous.activity) | previous.activity != activity1) |
                      activity_id == activity1 & (is.na(next.activity) | next.activity != activity2))) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | (!xoccurs & !yoccurs)) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs),
              first_activity = first(activity_id),
              yoccurs = last(yoccurs),
              chain = all(chain == TRUE)) %>%
    mutate(resp = chain | 
             (!xoccurs & !yoccurs))  %>%
    select(CASE_concept_name, resp)
}

alternate_response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | !xoccurs) %>%
    mutate(next.activity = lead(activity_id),
           alternate = activity_id != activity1 | (next.activity != activity1 | is.na(next.activity))) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs),
              alternate = all(alternate == TRUE)) %>%
    mutate(resp = (last_activity == activity2 & alternate) | !xoccurs)  %>%
    select(CASE_concept_name, resp)
}

alternate_precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | !yoccurs) %>%
    mutate(previous.activity = lag(activity_id),
           alternate = activity_id != activity2 | (previous.activity != activity2 | is.na(previous.activity))) %>%
    summarize(first_activity = first(activity_id),
              yoccurs = first(yoccurs),
              alternate = all(alternate == TRUE)) %>%
    mutate(resp = (first_activity == activity1 & alternate) | !yoccurs)  %>%
    select(CASE_concept_name, resp)
}

alternate_succession <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id,
           yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | (!xoccurs & !yoccurs)) %>%
    mutate(previous.activity = lag(activity_id),
           next.activity = lead(activity_id),
           last.activity = last(activity_id),
           first.activity = first(activity_id),
           alternate = (activity_id != activity2 & activity_id !=activity1) | 
            ((activity_id == activity2 & (previous.activity != activity2 | is.na(previous.activity)) |
            (activity_id == activity1 &  (next.activity != activity1 | is.na(next.activity)))
              ))) %>%
    summarize(first.activity = first(activity_id),
              xoccurs = first(xoccurs),
              last.activity = last(activity_id),
              yoccurs = last(yoccurs),
              alternate = all(alternate == TRUE)) %>%
    mutate(resp = (first.activity == activity1 & last.activity ==activity2 & alternate) | (!xoccurs&!yoccurs))  %>%
    select(CASE_concept_name, resp)
}

not_coexistence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(
      xcount = sum(activity_id == activity1),
      ycount = sum(activity_id == activity2)
    ) %>%
    mutate(resp = ((xcount == 0 & ycount == 0) | (xcount == 0 & ycount > 0) | (xcount > 0 & ycount == 0))) %>%
    select(CASE_concept_name, resp)
}

not_succession <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id) %>%
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | (!xoccurs & !yoccurs)) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs),
              first_activity = first(activity_id),
              yoccurs = last(yoccurs)) %>%
    mutate(resp = !xoccurs | !yoccurs | (last_activity != activity2 & first_activity != activity1))  %>%
    select(CASE_concept_name, resp)
}



constraints = c(
  "Responded Existence",
  "Response",
  "Precedence",
  "Chain Response",
  "Chain Precedence",
  "Init",
  "End",
  "Participation",
  "At Most Once",
  "Coexistence",
  "Succession",
  "Chain Succession",
  "Not Chain Succession",
  "Alternate Response",
  "Alternate Precedence",
  "Alternate Succession",
  "Not Coexistence",
  "Not Succession"
)

dual_constraints = c("Responded Existence",
                     "Response",
                     "Precedence",
                     "Chain Response",
                     "Chain Precedence",
                     "Coexistence",
                     "Succession",
                     "Chain Succession",
                     "Not Chain Succession",
                     "Alternate Response",
                     "Alternate Precedence",
                     "Alternate Succession",
                     "Not Coexistence",
                     "Not Succession")

ui <- fluidPage(
  titlePanel("Constraints Based Miner"),
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
          }")
        )
    ),
    fluidRow(selectInput("x", "None", label = "Activity A")),
    uiOutput("activity2_input"),
    fluidRow(actionButton(
      inputId = "act",
      label = "Add Constraint",
      icon("plus", lib = "glyphicon")
    )),
    fluidRow(DT::dataTableOutput("table")),
    fluidRow(actionButton(
      inputId = "export",
      label = "Export",
      icon("export", lib = "glyphicon")
    ))
    ),
  mainPanel(fluidPage(
    width = 12,
    #shinycssloaders::withSpinner(uiOutput("process", height = "800px"))
    withSpinner(processanimaterOutput("process", height = "800px"))
  ))
  
)


server <- function(input, output, session) {
  # counter value used as id for the releationships
  counter <- reactiveValues(countervalue = 0)

  filtered_events <- list()
  
  # Example data
  reactiveDataValues <-
    reactiveValues(
      constraints = data.frame(
        Id = NULL,
        ActivityA = NULL,
        ActivityB = NULL,
        Constraint = NULL,
        Filtered = NULL,
        Delete = NULL
      ),
      eventlog = NULL,
      filters = NULL
    )
  
  
  # remove UI for second activity if single constraint is selected
  output$activity2_input <- renderUI({
    if (input$z %in% dual_constraints) {
      if (is.null(reactiveDataValues$eventlog)) {
        possible_activities <- "None"
      } else {
        possible_activities <- unique(reactiveDataValues$eventlog$activity_id)
      }
      
      fluidRow(selectInput("y", possible_activities, label = "Activity B"))
    }
  })

  # read file only if fileinput changes
  observeEvent(input$xes_input, {
    reactiveDataValues$eventlog <- read_xes(input$xes_input$datapath)
    reactiveDataValues$filters <-
      data.frame(row.names = unique(reactiveDataValues$eventlog$CASE_concept_name))

    # set content of activity dropdown boxes
    possible_activities <- unique(reactiveDataValues$eventlog$activity_id)

    updateSelectInput(session = session,
                      inputId = "x",
                      choices = possible_activities)
    updateSelectInput(session = session,
                      inputId = "y",
                      choices = possible_activities)
  })
  
  # render graph
  output$process <- renderProcessanimater(expr = {

    if (is.null(reactiveDataValues$eventlog)) {
      return()
    }
    
    if (length(reactiveDataValues$filters) == 0) {
      event_filter <- data.frame(case_id = c())
    } else {
      event_filter <- data.frame(reactiveDataValues$filters)
      event_filter$alltrue <-
        rowSums(event_filter) == length(event_filter)
      event_filter <- event_filter %>%
        rownames_to_column("case_id") %>%
        filter(!alltrue)
    }
    
    filtered_events <<-
      reactiveDataValues$eventlog %>% filter(!CASE_concept_name %in% event_filter$case_id)
    
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
    
    #tagList(process_map(
    #  filtered_events
    #))
  })
  
  # action which is fired when pressing the Ok Button for inserting constraints
  observeEvent(input$act, {
    
    if(is.null(reactiveDataValues$eventlog))
      return()
    
    constraints <- reactiveDataValues$constraints
    # check if the constraint with the same activities already exists 
    if(input$z %in% constraints$Constraint && 
       input$x %in% constraints$ActivityA &&
       input$y %in% constraints$ActivityB)
    {
      showModal(modalDialog(
        title = "Constraint already added",
        paste0("The constraint for the selected activities has already been added."),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    # constraints
    constraint = switch(
      input$z,
      "Participation" = participation,
      "At Most Once" = at_most_once,
      "Init" = init_constraint,
      "End" = end_constraint,
      "Responded Existence" = responded_existence,
      "Response" = response,
      "Alternate Response" = alternate_response,
      "Chain Response" = chain_response,
      "Precedence" = precedence,
      "Alternate Precedence" = alternate_precedence,
      "Chain Precedence" = chain_precedence,
      "Coexistence" = coexistence,
      "Succession" = succession,
      "Alternate Succession" = alternate_succession,
      "Chain Succession" = chain_succession,
      "Not Chain Succession" = not_chain_succession,
      "Not Succession" = not_succession,
      "Not Coexistence" =  not_coexistence
    )
    
    if (input$z %in% dual_constraints) {
      tbl_activity_b <- input$y
      constraint_matches = constraint(reactiveDataValues$eventlog, input$x, input$y)
    } else {
      tbl_activity_b <- "-"
      constraint_matches = constraint(reactiveDataValues$eventlog, input$x)
    }
    
    # merge new constraints with old constraints
    constraint_matches <- constraint_matches %>%
      column_to_rownames("CASE_concept_name")
    colnames(constraint_matches) <- toString(counter$countervalue)
    
    reactiveDataValues$filters = merge(reactiveDataValues$filters, constraint_matches, by = "row.names") %>%
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
    reactiveDataValues$constraints <- rbind(reactiveDataValues$constraints, newrow)
  })
  
  observeEvent(input$export, {
    if(nrow(filtered_events) == 0){
      showModal(modalDialog(
        title = "Error",
        paste0("Process model does not contain data"),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    #fileName = paste0("./results/",Sys.time(),".xes", sep = "");
    export = data.frame(filtered_events);
    colnames(export)[which(colnames(export) == "CASE_concept_name")] <- "case_classifier"
    exportEventLog <- bupaR::eventlog(export, 
                    case_id = "case_classifier",
                    activity_id = "activity_id",
                    activity_instance_id = "activity_instance_id",
                    timestamp = "timestamp",
                    lifecycle_id = "lifecycle_id",
                    resource_id = "resource_id",
                    order = ".order")
    tryCatch({ write_xes(exportEventLog, xesfile = file.choose(new=TRUE))
      showModal(modalDialog(
        title = "Export",
        paste0("Process model has been successfully exported"),
        easyClose = TRUE,
        footer = NULL
      ))
      },
      error=function(err) {
        showModal(modalDialog(
          title = "Export",
          paste0("The file selection has been canceled."),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    
  })
  
  # print the table
  output$table <- DT::renderDataTable(
    reactiveDataValues$constraints,
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
    reactiveDataValues$filters[[toString(selectedId)]] <- NULL
    
    # Remove the value of the table
    reactiveDataValues$constraints <-
      reactiveDataValues$constraints[-which(reactiveDataValues$constraints$Id == selectedId),]
  })
  
  observeEvent(input$z, {
    output$constraintDescription <- renderText({
      switch(
        input$z,
        "Responded Existence" = "If Activity A occurs, then Activity B occurs too",
        "Response" = "If Activity A occurs, then Activity B occurs after A",
        "Alternate Response" = "If Activity A occurs, then Activity B occurs after A - another Activity A only occurs after Activity B",
        "Precedence" = "B occurs only if preceded by A",
        "Chain Response" = "If Activity A occurs, Activity B occurs immediately after it",
        "Chain Precedence" = "Activity B occurs only if Activity A occurs immediately before it",
        "Init" = "Activity A is the first to occur",
        "End" = "Activity A is the last to occur",
        "Participation" = "Activity A occurs at least once",
        "At Most Once" = "Activity A occurs at most once",
        "Coexistence" = "If Activity A or Activity B occurs, then the other one must also occur",
        "Succession" = "Every Activity A must be succeeded by Activity B, and every Activity B must be preceded by Activity A",
        "Chain Succession" = "Every Activity A must be succeeded by Activity B, and every Activity B must be preceded by Activity A and Activity A and Activity B must be next to each other",
        "Not Chain Succession" = "Activity A and Activity B can not occur after one another",
        "Alternate Precedence" = "B occurs only if preceded by A - another Activity B only occurcs bevore A",
        "Alternate Succession" = "Every Activity A must be succeeded by Activity B, and every Activity B must be preceded by Activity A - No repetition of either Activity A or Activity B is allowed",
        "Not Coexistence" = "Activity A and Activity B can not occur both",
        "Not Succession" = "Every Activity A can not be succeeded by Activity B, and every Activity B can not be preceded by Activity A"
        )
    })
  })
  
  # exportTestValues(RV = { RV$eventlog })
}

# Run the application
shinyApp(ui = ui, server = server)

#runApp()