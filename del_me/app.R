pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
  movies %>% 
  filter(year >= 2000) %>%
  select(title,year,length,rating,votes,Action:Short) %>% 
  gather(genre,value,Action:Short) %>% 
  filter(value == 1) %>% 
  select(-value)

# Get genre list
genres <- 
  shiny_movie_set %>% 
  distinct(genre) %>% 
  unlist(.)

names(genres) <- NULL
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Movie Length and IMDB Scores"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Years",
                  min = min(shiny_movie_set$year),
                  max = max(shiny_movie_set$year),
                  value = range(shiny_movie_set$year),step = 1),
      selectInput("genre","Genre",choices = c("All",genres),selected="All",multiple=TRUE),
      
      sliderInput("votes",
                  "At Least X votes",
                  min = min(shiny_movie_set$votes),
                  max = max(shiny_movie_set$votes),
                  value = min(shiny_movie_set$votes))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("moviePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  output$moviePlot <- renderPlot({
    new_df <- shiny_movie_set %>% filter(votes >= input$votes) %>% filter(year >= input$years[1] & year <= input$years[2])
    
    if(!("All" %in% input$genre)){
      new_df <- new_df %>% filter(genre %in% input$genre)
    }
    
    ggplot(new_df,aes(x=length,y=rating,color=genre)) + geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)