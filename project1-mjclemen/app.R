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

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- fluidPage(
   
   # Application title
   titlePanel("Journalist Deaths since 1992"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput(inputId = "")
      ),
      
      
      mainPanel(
         
      )
   )
)

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
   


}

# Run the application 
shinyApp(ui = ui, server = server)

