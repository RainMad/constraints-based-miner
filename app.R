library(shiny)
library(DT)
library(bupaR)
library(shinycssloaders)
library(processanimateR)
library(processmapR)
library(xesreadR)

options(shiny.maxRequestSize=1*1024^3) # upload limit = 1GB

### constraints

responded_existence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(xcount = sum(activity_id == activity1),
              ycount = sum(activity_id == activity2)) %>%
    mutate(responded_existence = xcount == 0 | ycount > 0) %>%
    pull(responded_existence)
}

response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>% 
    mutate(xoccurs = activity1 %in% activity_id) %>%
    filter(activity_id == activity1 | activity_id == activity2 | !xoccurs) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs)) %>%
    mutate(resp = last_activity == activity2 | !xoccurs) %>%
    pull(resp)
}


precedence <- function(eventlog, activity1, activity2) {
  
  eventlog %>%
    group_by(CASE_concept_name) %>% 
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 | activity_id == activity2 | !yoccurs) %>%
    summarize(first_activity = first(activity_id),
              yoccurs = first(yoccurs)) %>%
    mutate(resp = first_activity == activity1 | !yoccurs) %>%
    pull(resp)
}


chain_response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(next.activity = lead(activity_id),
           response = activity_id == activity1 & next.activity != activity2) %>%
    summarize(response_summarize = sum(response),
              xExists = activity1 %in% activity_id,
              yExists = activity2 %in% activity_id) %>%
    mutate(resp = xExists == TRUE & yExists == TRUE & response_summarize == 0) %>%
    pull(resp)
}


chain_precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(previous.activity = lag(activity_id),
           response = activity_id == activity2 & previous.activity != activity1) %>%
    summarize(response_summarize = sum(response),
              xExists = activity1 %in% activity_id,
              yExists = activity2 %in% activity_id) %>%
    mutate(resp = xExists == TRUE & yExists == TRUE & response_summarize == 0) %>%
    pull(resp)
}


constraints = c("Responded Existence",
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
    fluidRow(
      selectInput("y", "None", label = "Activity B")
      ),
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
  
  observeEvent(input$xes_input, {
    RV$eventlog <- read_xes(input$xes_input$datapath)
    RV$filters <- data.frame(row.names = unique(RV$eventlog$CASE_concept_name))
    RV$constraints <- NULL
    
    # set content of activity dropdown boxes
    possible_activities <- unique(RV$eventlog$activity_id)
    
    updateSelectInput(session = session, inputId = "x", choices=possible_activities)
    updateSelectInput(session = session, inputId = "y", choices=possible_activities)
  })
  
  output$process <- renderProcessanimater(expr = {
    if (is.null(RV$eventlog)) {
      return()
    }
    
    # create graph
    
    if (length(RV$filters) == 0) {
      event_filter <- data.frame(case_id = c()) 
    } else {
      event_filter <- RV$filters %>% 
        mutate(alltrue = rowSums(.) == length(.)) %>%
        rownames_to_column("case_id") %>%
        filter(!alltrue)
    }
    
    print("event filter")
    print(event_filter)
    
    animate_process(
      RV$eventlog %>% filter(!CASE_concept_name %in% event_filter$case_id),
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
                        "Chain Precedence" = chain_precedence)
    constraint_matches = constraint(RV$eventlog, input$x, input$y)
    print("matches")
    print(constraint_matches)
    RV$filters[[toString(counter$countervalue)]] = constraint_matches

    # create a new entry
    newrow = data.frame(
      id = counter$countervalue,
      activity1 = input$x,
      activity2 = input$y,
      constraint = input$z,
      filtered = paste0(round((1 - sum(constraint_matches) / length(constraint_matches)) * 100, 1), "%"),
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
    
    print(isolate(RV$constraints$id))
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
    print(paste("deleted Id:", selectedId))
    # Remove the value of the table
    RV$constraints <<- RV$constraints[-which(RV$constraints$id == selectedId), ]
  })
  
  observeEvent(input$sliderData, {
    print(input$sliderData)
  })
  
  
  print(isolate(RV$constraints))
}

# Run the application
shinyApp(ui = ui, server = server)
