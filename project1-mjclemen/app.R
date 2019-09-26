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

# Place application title in header of dashboard
app.header <- dashboardHeader(
  title = "Journalist Deaths 1992 - 2019", titleWidth = 300
  )

app.sidebar <- dashboardSidebar(
  # Change sidebar width to match the title width
  width = 300,
  
  sidebarMenu(id = "tabs",
    
    menuItem("Datatable", tabName = "datatable", icon = icon("th")),
    menuItem("Country Stats", tabName = "country_stats", icon = icon("th")),
    menuItem("Demographic Stats", tabName = "demographic_stats", icon = icon("th")),
    menuItem("Hostage Stats", tabName = "hostage_stats", icon = icon("th")),
    
    # Select what type of death to plot ------------------------
    checkboxGroupInput(inputId = "selected.impunity",
                       label = "Select whether impunity was granted to view:",
                       choices = sort(unique(deaths$`Impunity  for Murder`)),
                       selected = "Yes"),
    
    # Select what type of medium to plot ------------------------
    radioButtons(inputId = "selected.medium",
                 label = "Select what type of medium (of the journalist) to view:",
                 choices = c("Internet", "Print", "Radio", "Television"),
                 selected = "Television"),
    
    # Select what years of data to plot ------------------------
    sliderInput(inputId = "selected.year",
                label = "Select which year(s) to view:",
                min = min(deaths$`Year of Death`),
                max = max(deaths$`Year of Death`),
                value = c(1995,2019),
                step = 1,
                sep = "")
    )
  )

app.body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "datatable",
            fluidRow(
              column(
                # Show data table ---------------------------------------------
                dataTableOutput(outputId = "deathstable"), width = 8
              )
              )
            ),
    tabItem(tabName = "country_stats",
            # Show info box ---------------------------------------------
            uiOutput(outputId = "country.deaths")
            ),
    tabItem(tabName = "demographic_stats",
            # Show info box ---------------------------------------------
            uiOutput(outputId = "sex.deaths")
            ),
    tabItem(tabName = "hostage_stats",
            # Show info box ---------------------------------------------
            fluidRow(
              uiOutput(outputId = "captive")),
            fluidRow(
              plotOutput(outputId = "coverage.over.year", click = "boxplot_click")),
            fluidRow(
              textOutput(outputId = "boxplot.y.info"))
            )
    )
  )

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- dashboardPage(
  header = app.header,
  sidebar = app.sidebar,
  body = app.body,
  # Set the header color
  skin = "green"
)

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
  
  # Create subset of deaths dataset to account for user input. Specifically, year range, selected medium
  # of the journalist, and whether the murder was impunity.
  deaths_subset <- reactive({
    deaths <- subset(deaths,
                     `Impunity  for Murder` %in% input$selected.impunity &
                       `Year of Death` >= input$selected.year[1] & `Year of Death` <= input$selected.year[2]
                     )
     deaths <- filter(deaths,grepl(input$selected.medium,Medium))
  })
  
  # Country with the most deaths info box ----------------------------------------------
  output$country.deaths <- renderUI({
    ds <- deaths_subset()
    highest <- tail(names(sort(table(ds$`Country Killed`))), 1)
    infoBox("Country with the most deaths", value = highest, color = "green", width = 5)
  })
  
  # Sex with the least deaths info box ----------------------------------------------
  output$sex.deaths <- renderUI({
    ds <- deaths_subset()
    lowest <- tail(names(sort(table(ds$Sex))), 2)
    infoBox("Sex with the fewest deaths", value = lowest, color = "green", width = 5)
  })
  
  # Count how many were taken captive and put in value box ------------------------------
  output$captive <- renderUI({
    ds <- deaths_subset()
    captive <- table(str_trim(ds$`Taken Captive`))
    count.captive <- captive[names(captive) == "Yes"]
    infoBox(title = "Taken captive by an organization before death:", value = count.captive, color = "green", width = 5)
  })
  
  output$type.over.year <- renderPlot({
    ds <- deaths_subset()
    ggplot(ds, aes(x = ds$`Type of Death`, y = ds$`Year of Death`)) + geom_boxplot()
  })
  
  output$coverage.over.year <- renderPlot({
    ds <- deaths_subset()
    ds_split <- setDT(ds)[, strsplit(str_trim(as.character(Coverage)), ",", fixed=TRUE), by = .(`Year of Death`, Coverage)
                        ][,.(Coverage = V1, `Year of Death`)]
    
    ds_split$Coverage <- as.factor(str_trim(ds_split$Coverage))
    ds_split$`Year of Death` <- as.integer(ds_split$`Year of Death`)
    
    ggplot(ds_split, aes(x = ds_split$Coverage, y = ds_split$`Year of Death`)) + geom_boxplot()
  })
  
  output$boxplot.y.info <- renderText({
    if (is.null(input$boxplot_click$y)) {
      return("")
    } else {
      year <- round(input$boxplot_click$y)
      HTML("You've selected the year: ", year)
    }
    #paste0("y=", round(input$boxplot_click$y))
  })
   
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deaths_subset(), options = list(orderClasses = TRUE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

