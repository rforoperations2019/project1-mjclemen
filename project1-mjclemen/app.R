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
library(data.table)

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
    
    menuItem("User Filtered Data", tabName = "datatable", icon = icon("table")),
    menuItem("Place of Death", tabName = "location_stats", icon = icon("th")),
    menuItem("Journalist Demographics", tabName = "demographic_stats", icon = icon("th")),
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
              # Show data table ---------------------------------------------
              dataTableOutput(outputId = "deathstable"))
            ),
    tabItem(tabName = "location_stats",
            fluidRow(
              # Show info box ---------------------------------------------
              uiOutput(outputId = "country.deaths"),
              textOutput(outputId = "dotplot.x.info"),
              textOutput(outputId = "dotplot.y.info")),
            fluidRow(
              plotOutput(outputId = "coverage.per.country", hover = "dotplot_hover"))
            ),
    tabItem(tabName = "demographic_stats",
            # Show info box ---------------------------------------------
            uiOutput(outputId = "sex.deaths"),
            # Show user input radio buttons to select how to fill the bar plot
            # uiOutput(outputId = "barplot.fill"),
            # Show barplot, showing the number of deaths per journalist nationality
            plotOutput(outputId = "barplot.nationality")
            ),
    tabItem(tabName = "hostage_stats",
            # Show info box ---------------------------------------------
            fluidRow(
              valueBoxOutput(outputId = "captive")
              )
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
  
  # Country with the most deaths value box ----------------------------------------------
  # Note: I used renderUI intead of renderValueBox because width only works in prior
  output$country.deaths <- renderUI({
    ds <- deaths_subset()
    # Get the number of deaths for each country, sort it, and extract the one with the highest count
    highest.country <- names(tail(sort(table(ds$`Country Killed`)), 1))
    valueBox(value = highest.country, subtitle = "Has the Most Deaths", color = "green", width = 3)
  })
  
  # Number of male deaths value box ----------------------------------------------
  # Note: I used renderUI intead of renderValueBox because width only works in prior
  output$sex.deaths <- renderUI({
    ds <- deaths_subset()
    # Get the count of deaths for both sexes
    sex.counts <- table(ds$Sex)
    # Extract the count of deaths for males to display in dashboard
    male.count <- sex.counts[names(sex.counts) == "Male"]
    valueBox(value = male.count, subtitle = "Male Deaths", color = "green", width = 3)
  })
  
  # Number of journalists taken captive value box ------------------------------
  output$captive <- renderValueBox({
    ds <- deaths_subset()
    # Get the count of journalists taken captive and those not taken captive
    captive <- table(str_trim(ds$`Taken Captive`))
    # Extract the count of only those taken captive to display in dashboard
    count.captive <- captive[names(captive) == "Yes"]
    valueBox(value = count.captive, subtitle = "Journalists Taken Captive Before Death", color = "green")
  })
  
  # Plot the topic that the journalists covered over the years
  output$coverage.per.country <- renderPlot({
    ds <- deaths_subset()
    ds_split <- setDT(ds)[, strsplit(str_trim(as.character(Coverage)), ",", fixed=TRUE), by = .(`Country Killed`, Coverage)
                        ][,.(Coverage = V1, `Country Killed`)]
    
    ds_split$Coverage <- as.factor(str_trim(ds_split$Coverage))
    ds_split$`Country Killed` <- as.factor(str_trim(ds_split$`Country Killed`))
    
    ggplot(ds_split, aes(x = ds_split$Coverage, y = ds_split$`Country Killed`)) +
      geom_dotplot(binaxis='y', 
                   stackdir='center', 
                   dotsize = .5, 
                   fill="green") +
      labs(x = "Journalists' Assignment Topic", y = "Country Killed", title = "Journalist Topic Coverage in relation to Place of Death")
  })
  
  # Make dotplot interactive by adding hover feature. When hovering over the dotplot, the year will be displayed to user
  output$dotplot.y.info <- renderText({
    if (is.null(input$dotplot_hover)) {
      return("")
    } else {
      ds <- deaths_subset()
      ds_split <- setDT(ds)[, strsplit(str_trim(as.character(Coverage)), ",", fixed=TRUE), by = .(`Country Killed`, Coverage)
                            ][,.(Coverage = V1, `Country Killed`)]
      ds_split$`Country Killed` <- as.factor(str_trim(ds_split$`Country Killed`))
      country.levels <- levels(ds_split$`Country Killed`)
      country <- country.levels[round(input$dotplot_hover$y)]
      paste0("You've selected the country: ", country)
    }
  })
  
  # Make dotplot interactive by adding hover feature. When hovering over the dotplot, the year will be displayed to user
  output$dotplot.x.info <- renderText({
    if (is.null(input$dotplot_hover)) {
      return("")
    } else {
      ds <- deaths_subset()
      ds_split <- setDT(ds)[, strsplit(str_trim(as.character(Coverage)), ",", fixed=TRUE), by = .(`Country Killed`, Coverage)
                            ][,.(Coverage = V1, `Country Killed`)]
      
      ds_split$Coverage <- as.factor(str_trim(ds_split$Coverage))
      coverage.levels <- levels(ds_split$Coverage)
      coverage <- coverage.levels[round(input$dotplot_hover$x)]
      paste0("You've selected the journalist topic: ", coverage)
    }
  })
  
  # output$barplot.fill <- renderUI({
  #   radioButtons(inputId = "choose.fill", label = "Choose How to Fill the Bar Plot",
  #                choices = c("Freelance", "Tortured", "Threatened"), selected = "Freelance")
  # })
  
  output$barplot.nationality <- renderPlot({
    ds <- deaths_subset()
    top.nationalities <- names(tail(sort(table(ds$Nationality)),10))
    ggplot(ds, aes(x = Nationality, fill = Freelance)) + geom_bar() +
      scale_x_discrete(limits = top.nationalities) + scale_fill_brewer(palette = "Accent") +
      labs(x = "Journalist Nationality", y = "Number of Journalist Deaths", title = "Journalist Death by Nationality")
  })
   
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deaths_subset(), options = list(orderClasses = TRUE, autoWidth = FALSE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

