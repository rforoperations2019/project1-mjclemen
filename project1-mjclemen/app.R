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
  # Set the header color
  skin = "green",
   
   # Place application title in header of dashboard
  app.header <- dashboardHeader(
   title = "Journalist Deaths 1992 - 2019",

   # Make space for the title
   titleWidth = 300
   ),
  
  app.sidebar <- dashboardSidebar(
    # Change sidebar width to match the title width
    width = 300,
    
        # Select what type of death to plot ------------------------
        checkboxGroupInput(inputId = "selected.impunity",
                           label = "Select whether impunity was granted to view in Data Table:",
                           choices = sort(unique(deaths$`Impunity  for Murder`)),
                           selected = "Partial"),
        
        # Select what type of medium to plot ------------------------
        radioButtons(inputId = "selected.medium",
                     label = "Select what type of medium to view in Data Table:",
                     choices = c("Internet", "Print", "Radio", "Television"),
                     selected = "Internet"),
        
        sliderInput(inputId = "selected.year",
                    label = "Select which year to view in Data Table:",
                    min = min(deaths$`Year of Death`),
                    max = max(deaths$`Year of Death`),
                    value = c(1995,2019),
                    step = 1,
                    sep = "")
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
                     `Impunity  for Murder` %in% input$selected.impunity &
                     Medium %in% input$selected.medium &
                       `Year of Death` >= input$selected.year[1] & `Year of Death` <= input$selected.year[2]
                     )
  })
  
  # Country with the most deaths info box ----------------------------------------------
  output$country.deaths <- renderInfoBox({
    ds <- deaths_subset()
    highest <- count(ds,ds$`Country Killed`)
    infoBox("Country with the most deaths", value = highest, color = "green")
  })
   
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deaths_subset(), options = list(orderClasses = TRUE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

