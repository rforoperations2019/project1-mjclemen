library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(stringr)
library(tools)
library(rlist)
library(scales)

deaths <- read.csv("JournalistDeaths.csv")

# Rename column names that have "." separating words
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")

# deaths$`Year of Death` <- as.Date(as.character(deaths$`Year of Death`), format = "%Y")

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- dashboardPage(
   
   # Place application title in header of dashboard
  app.header <- dashboardHeader(
   title = "Journalist Deaths 1992 - 2019"
   ),
  
  app.sidebar <- dashboardSidebar(
        # Select what type of death to plot ------------------------
        checkboxGroupInput(inputId = "selected.death.type",
                           label = "Select Type of Death(s) to view in Data Table:",
                           choices = sort(unique(deaths$`Type of Death`)),
                           selected = "Murder"),
        
        # Select what type of medium to plot ------------------------
        radioButtons(inputId = "selected.medium",
                     label = "Select which medium to view in Data Table:",
                     choices = c("Internet", "Print", "Radio", "Television"),
                     selected = "Print"),
        
        sliderInput(inputId = "selected.year",
                    label = "Select which year to view in Data Table:",
                    min = min(deaths$`Year of Death`),
                    max = max(deaths$`Year of Death`),
                    value = c(1995,2019))
      ),
      
      
      app.body <- dashboardBody(
        # Show data table ---------------------------------------------
        dataTableOutput(outputId = "deathstable")
      )
   )

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
  
  deaths_subset <- reactive({
    deaths <- subset(deaths,
                     `Type of Death` %in% input$selected.death.type &
                     Medium %in% input$selected.medium &
                    `Year of Death` >= input$selected.year[1] & `Year of Death` <= input$selected.year[2]
                     )
  })
   
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deaths_subset(), options = list(orderClasses = TRUE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

