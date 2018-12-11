#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("UI"),
   sidebarPanel(
     fluidRow(
     sliderInput("data", "% of Data", min=0, max = 100, post="%", value=90)),
     fluidRow(
       column(4,
              selectInput("x", "X", choices = c("A", "B", "C"), label = NULL)),
       column(4,
              selectInput("xy", "X", choices = c("s", "<-", "->"), label = NULL)),
       column(4,
              selectInput("y", "Y", choices = c("A", "B", "C"), label = NULL))
       ),
     fluidRow(
     column(4, 
            actionButton(inputId = "act", label = "Ok", icon("triangle-right", lib="glyphicon")))),
     fluidRow(
       column(12,
              DT::dataTableOutput("table"))
     )
   ),
   mainPanel("Output")
)


# Example data
RV <- reactiveValues(data = data.frame(id=NULL, x=NULL, y=NULL, xy=NULL, Delete=NULL, stringsAsFactors = FALSE), rowname=NULL)


server <- function(input, output) {
  # counter value used as id for the releationships
  counter <- reactiveValues(countervalue = 0) 
   
  # action which is fired when pressing the Ok Button for inserting constraints
   observeEvent(input$act, {
     # create a new entry
     newrow = data.frame(id=counter$countervalue,
                        x=input$x, 
                        y=input$y, 
                        xy=input$xy, 
                        Delete=paste("<button id='button_",counter$countervalue, "' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>Delete</button>", sep=""),
                        stringsAsFactors = FALSE,
                        row.names = NULL)
     # increase the id
     counter$countervalue = counter$countervalue + 1
     # add the entry to the table
     RV$data <<- rbind(RV$data, newrow)
     
     print(isolate(RV$data$id))
   })  
   
    # print the table
    output$table <- DT::renderDataTable(
      RV$data, escape = FALSE, options = list(
        pageLength = 10,
        dom = 't',
        ordering = F,
        autoWidth = TRUE,
        columnDefs = list(list(width = '15px', targets = c(1,2,3,4))))
      )
   
    # if the select button of one entry of the table has been pressed, delete the entry
   observeEvent(input$select_button, {
     # get the id of the button which is the same like in the data frame
     selectedId <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
     print(paste("Selected Id: " , selectedId))
     # Remove the value of the table
     RV$data <<- RV$data[-which(RV$data$id==selectedId),]
   })
   
   print(isolate(RV$data))
}

# Run the application 
shinyApp(ui = ui, server = server)

