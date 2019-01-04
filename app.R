library(shiny)
library(DT)
library(bupaR)
library(shinycssloaders)
library(processanimateR)
library(processmapR)
library(xesreadR)

options(shiny.maxRequestSize=1*1024^3) # upload limit = 1GB

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("UI"),
  sidebarPanel(
    fluidRow(
      fileInput("xes_input", "Choose XES File",
                accept = c("text/xes", ".xes"))
    ),
    fluidRow(
      sliderInput(
        "sliderData",
        "% of Data",
        min = 0,
        max = 100,
        post = "%",
        value = 90
      )
    ),
    fluidRow(
      column(4,
             selectInput(
               "x", "X", choices = c("A", "B", "C"), label = NULL
             )),
      column(4,
             selectInput(
               "xy",
               "X",
               choices = c("s", "<-", "->"),
               label = NULL
             )),
      column(4,
             selectInput(
               "y", "Y", choices = c("A", "B", "C"), label = NULL
             ))
    ),
    fluidRow(column(
      4,
      actionButton(
        inputId = "act",
        label = "Ok",
        icon("triangle-right", lib = "glyphicon")
      )
    )),
    fluidRow(column(12,
                    DT::dataTableOutput("table")))
  ),
  mainPanel("Output",
            fluidPage(
              width = 20,
              shinycssloaders::withSpinner(processanimaterOutput("process"))
            ))
)


# Example data
RV <-
  reactiveValues(
    data = data.frame(
      id = NULL,
      x = NULL,
      y = NULL,
      xy = NULL,
      Delete = NULL,
      stringsAsFactors = FALSE
    ),
    rowname = NULL
  )

server <- function(input, output) {
  # counter value used as id for the releationships
  counter <- reactiveValues(countervalue = 0)
  

  #data12 <- reactiveValues(data = data)
  
  #data <- data[which(data$CASE_concept_name!="erewrwr"),]
  
  output$process <- renderProcessanimater(expr = {
    if (is.null(input$xes_input)) {
      data <- NULL
    }
    else {
      print(paste0("Reading ", input$xes_input$datapath))
      data <- read_xes(input$xes_input$datapath)
      
      animate_process(
        data,
        mode = "off",
        timeline = TRUE,
        legend = "color",
        initial_state = "paused"
      )
    }
  })
  
  #output$process <- renderPlot(data)
  
  # action which is fired when pressing the Ok Button for inserting constraints
  observeEvent(input$act, {
    # create a new entry
    newrow = data.frame(
      id = counter$countervalue,
      x = input$x,
      y = input$y,
      xy = input$xy,
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
    RV$data <<- rbind(RV$data, newrow)
    
    print(isolate(RV$data$id))
  })
  
  # print the table
  output$table <- DT::renderDataTable(
    RV$data,
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
    print(paste("Selected Id: " , selectedId))
    # Remove the value of the table
    RV$data <<- RV$data[-which(RV$data$id == selectedId), ]
  })
  
  observeEvent(input$sliderData, {
    print(input$sliderData)
    #data12$data <- data12$data %>% filter(CASE_concept_name == "2")
  })
  
  
  print(isolate(RV$data))
}

# Run the application
shinyApp(ui = ui, server = server)
