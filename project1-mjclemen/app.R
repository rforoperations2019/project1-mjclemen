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

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- fluidPage(
   
   # Application title
   titlePanel("Journalist Deaths 1992 - 2019"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Select what type of death to plot ------------------------
        checkboxGroupInput(inputId = "selected.death.type",
                           label = "Select Type of Death(s) to view in Data Table:",
                           choices = sort(unique(deaths$`Type of Death`)),
                           selected = c("Public Works", "Finance")),
        
        # Select what type of medium to plot ------------------------
        radioButtons(inputId = "selected.medium",
                     label = "Select which medium to view in Data Table:",
                     choices = c("Internet", "Print", "Radio", "Television"),
                     selected = "Print")
      ),
      
      
      mainPanel(
        # Show data table ---------------------------------------------
        dataTableOutput(outputId = "deathstable")
      )
   )
)

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
   
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deaths, options = list(orderClasses = TRUE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

