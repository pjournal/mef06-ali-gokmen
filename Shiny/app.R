#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

students <- readRDS("students.rds")

new_cols <- c("university","type","province","nationality","male","female","total")
colnames(students) <- new_cols

students <- na.omit(students)

students <- students %>% mutate(`female` = as.numeric(`female`))
students <- students %>% mutate(`male` = as.numeric(`male`))
students <- students %>% mutate(`total` = as.numeric(`total`))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Foreign Students by Nationality"),
    theme = shinythemes::shinytheme("readable"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("in_province", 
                    "Province",
                    choices = sort(unique(students$province)), 
                    selected = "İSTANBUL"
        ),
        
        selectInput("in_type",
                    "University Type",
                    choices = "",
                    selected = ""
        ),
        selectInput("in_name",
                    "University",
                    choices = "",
                    selected = "")
      ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Table", DT::dataTableOutput(outputId = "students_"))
          
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  observeEvent(input$in_province, {
    updateSelectInput(session, "in_type", choices = unique(students$type[students$province == input$in_province])
    )})
  
  observeEvent(c(input$in_province, input$in_type),{
    updateSelectInput(session, "in_name", choices = unique(students$university[students$province == input$in_province & students$type == input$in_type]))
  })
  
  students_ <- reactive({
    filter(students, (province %in% input$in_province) &
             (type %in% input$in_type) &
             (university %in% input$in_name)) %>%
      arrange(desc(total))
  })
  
  output$students_ <- DT::renderDataTable({
    DT::datatable(data = students_(),
                  options = list(pageLength = 20),
                  rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
