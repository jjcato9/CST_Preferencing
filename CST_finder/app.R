#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

## load in packages and data here
library(ggplot2)
library(maps)
library(tidyverse)
library(plotly)
source('job_filter.R')
source('region_filter.R')
source('map_builder.R')

data <- read.csv('data/CST_data.csv')


# Define UI ----
ui <- fluidPage(
  titlePanel("CST Preferences"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("theme", h3("Select Themes"), 
                  choices = list("All" = "All",
                                 "Cardiothoracics",
                                 "Oral & Maxillofacial Surgery",
                                 "ENT",
                                 "Trauma & Orthopaedics",
                                 "General Surgery",
                                 "Plastic Surgery",
                                 "Paediatric Surgery",
                                 "Urology",
                                 "Vascular",
                                 "Breast",
                                 "ITU"
                                 ),
                  selected = 'All'),
      checkboxGroupInput("region", h3("Select Regions"), 
                  choices = list("All" = "All",
                                 "East Midlands",
                                 "East of England",
                                 "Kent, Surrey and Sussex",
                                 "London",
                                 "North East",
                                 "North West",
                                 "South West",
                                 "Thames Valley",
                                 "Wessex",
                                 "West Midlands",
                                 "Yorkshire and the Humber",
                               "Wales",
                               "Scotland"),
                  selected = 'All'),
    submitButton("Submit")),
    mainPanel(
      h3('Explore The Map'),
      textOutput('selection'),
      plotlyOutput('map'),
      h3('Jobs Matching Preferences'),
      tableOutput('table')
    
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  #get df with filters applied
  df <- reactive(
    {req(input$theme)
      req(input$region)
      new <- job_filter(data,input$theme)
      new <- region_filter(new,input$region)})
  
  
  
  #State number of jobs which meet criteria
  output$selection <- renderText({
    paste('There are ',nrow(df()),' jobs which meet your criteria')
  })
  
  #Map locating jobs
  output$map <- renderPlotly({
    map_builder(df())
  }
  )
  
  #DF of jobs
  output$table <- renderTable(df() %>% select(1:4))
}

# Run the app ----
shinyApp(ui = ui, server = server)
