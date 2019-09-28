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

# Read in the file on journalist deaths ---------------------------------------------------
deaths <- read.csv("JournalistDeaths.csv")

# Rename column names that have "." separating words --------------------------------------
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")

# Place application title in header of dashboard ------------------------------------------
app.header <- dashboardHeader(
  title = "Journalist Killings 1992 - 2019", titleWidth = 300
  )

# Place user inputs and tab options in a sidebar to be displayed in dashboard
app.sidebar <- dashboardSidebar(
  
  # Change sidebar width to match the title width -----------------------------------------
  width = 300,
  
  # Create four tab options to place the datatable, the 3 valueboxes, and 3 plots
  # Also place user input controls below the tab options ----------------------------------
  sidebarMenu(id = "tabs",
    
    menuItem("User Filtered Data", tabName = "datatable", icon = icon("fas fa-table")),
    menuItem("Place of Death", tabName = "location_stats", icon = icon("fas fa-globe-americas")),
    menuItem("Journalist Demographics", tabName = "demographic_stats", icon = icon("fas fa-id-card")),
    menuItem("Details of Murder", tabName = "murder_stats", icon = icon("fas fa-dizzy")),
    
    # When the user is looking at the demographic tab, they will be given a chance to interact with the barplot
    conditionalPanel(
      condition = "input.tabs == 'demographic_stats'",
      selectInput("fill.choice", "Choose how to fill the Nationality Barplot:", 
                  choices = c("Worked Freelance" = "Freelance",
                              "Threatened before Death" = "Threatened",
                              "Tortured before Death" = "Tortured"))
    ),
    
    # When the user is looking at the murder tab, they will be given a chance to interact with the density plot
    conditionalPanel(
      condition = "input.tabs == 'murder_stats'",
      sliderInput(inputId = "adjust.choice",
                  label = "Adjust the bandwidth of the Density Plot to get a more atomistic or holistic view:",
                  min = 1,
                  max = 6,
                  value = 3)
    ),
    
    # Select whether impunity for the murder was granted to plot -----------------------------
    checkboxGroupInput(inputId = "selected.impunity",
                       label = "Select whether impunity was granted:",
                       choices = sort(unique(deaths$`Impunity  for Murder`)),
                       selected = c("No", "Yes")),
    
    # Select what type of medium to plot -----------------------------------------------------
    radioButtons(inputId = "selected.medium",
                 label = "Select a type of medium (of the journalist):",
                 choices = c("Internet", "Print", "Radio", "Television"),
                 selected = "Television"),
    
    # Select what years of data to plot ------------------------------------------------------
    sliderInput(inputId = "selected.year",
                label = "Select which year(s) to view:",
                min = min(deaths$`Year of Death`),
                max = max(deaths$`Year of Death`),
                value = c(1995,2019),
                step = 1,
                sep = "")
    )
  )

# Display 4 tabs: 1 containing the datatable and the other 3 each containing a valuebox and a plot
app.body <- dashboardBody(
  
  theme = shinytheme("readable"),
  
  tabItems(
    tabItem(tabName = "datatable",
            fluidRow(
                     # Show data table filtered based on user input -------------------------
                     box(title = "Selected Journalist Deaths Data",
                         dataTableOutput(outputId = "deathstable"),
                         width = 12)
              )
            ),
    tabItem(tabName = "location_stats",
            fluidRow(
              column(12,
                     # Show info box depicting the country with the most deaths -------------
                     uiOutput(outputId = "country.deaths"),
                     textOutput(outputId = "dotplot.x.info"),
                     textOutput(outputId = "dotplot.y.info"))
              ),
            fluidRow(
              column(12,
                     # Plot the journalist's coverage against where he/she was killed
                     # Show the user what coverage and country he/she is hovering over
                     plotOutput(outputId = "coverage.per.country", hover = "dotplot_hover"))
              )
            ),
    tabItem(tabName = "demographic_stats",
            fluidRow(
              column(12,
                     # Show info box depicting the number of male deaths ---------------------
                     uiOutput(outputId = "sex.deaths")
              )
            ),
            fluidRow(
              column(12,
                     # Show barplot, showing the number of deaths per journalist nationality
                     plotOutput(outputId = "barplot.nationality")
                     )
              )
            ),
    tabItem(tabName = "murder_stats",
            fluidRow(
              column(12,
                     # Show info box depicting the number of jouranlists taken captive
                     valueBoxOutput(outputId = "captive")
                     )
              ),
            fluidRow(
              column(12,
                     # Plot density plot to show death of the years
                     # Grouped by the source of murder
                     plotOutput(outputId = "source.by.year")
                     )
              )
            )
    )
  )

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- dashboardPage(
  header = app.header,
  sidebar = app.sidebar,
  body = app.body,
  skin = "black"
  )

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
  
  # Create subset of deaths dataset to account for user input. Specifically, year range, selected medium
  # of the journalist, and whether the murder was granted impunity ----------------
  deathsSubset <- reactive({
    deaths <- subset(deaths,
                     `Impunity  for Murder` %in% input$selected.impunity &
                       `Year of Death` >= input$selected.year[1] & `Year of Death` <= input$selected.year[2]
                     )
     deaths <- filter(deaths,grepl(input$selected.medium,Medium))
  })
  
  # Take the user filtered data and split up comma separated coverages in records 
  # Match these split records to the country the journalist was killed -------------
  dsSplitCoverage <- reactive({
    req(nrow(deathsSubset()) > 3)
    ds.coverage <- setDT(deathsSubset())[, strsplit(str_trim(as.character(Coverage)), ",", fixed=TRUE),
                           by = .(`Country Killed`, Coverage)
                           ][,.(Coverage = V1, `Country Killed`)]
    
    # Have both columns be factors to ensure levels for plotting -------------------
    ds.coverage$Coverage <- as.factor(str_trim(ds.coverage$Coverage))
    ds.coverage$`Country Killed` <- as.factor(str_trim(ds.coverage$`Country Killed`))
    
    top.countries <- names(tail(sort(table(ds.coverage$`Country Killed`)), 20))
    ds.coverage <- filter(ds.coverage,`Country Killed` %in% top.countries)
  })
  
  # Country with the most deaths value box -----------------------------------------
  # Note: I used renderUI intead of renderValueBox because width only works in prior
  output$country.deaths <- renderUI({
    ds <- deathsSubset()
    # Get the number of deaths for each country, sort it, and extract the one with the highest count
    highest.country <- names(tail(sort(table(ds$`Country Killed`)), 1))
    valueBox(value = highest.country, subtitle = "Has the Most Deaths", color = "blue", width = 4)
  })
  
  # Plot the journalist assignment topic over the years --------------------------
  output$coverage.per.country <- renderPlot({
    # Read in the reactive subset that has been split on coverage ----------------
    ds <- dsSplitCoverage()
    req(nrow(ds) > 3)
    # Drop levels after filtering out some categories, then re-factor the column 
    ds$`Country Killed` <- droplevels(ds$`Country Killed`)
    ds$`Country Killed` <- as.factor(str_trim(ds$`Country Killed`))
    
    ggplot(ds, aes(x = ds$Coverage, y = ds$`Country Killed`)) +
      geom_dotplot(binaxis='y',
                   stackdir='center',
                   dotsize = .5,
                   fill="blue") + 
      labs(x = "Journalist Assignment Topic", y = "Country Killed",
           title = "Frequency of Journalist Assignment Topic in Place of Death",
           subtitle = "Countries with the most deaths are shown")
  })
  
  # Make dotplot interactive by adding hover feature ------------------------------
  # When hovering over the dotplot, the assignment topic will be displayed to user
  output$dotplot.x.info <- renderText({
    if (is.null(input$dotplot_hover)) {
      return("")
    } else {
      ds <- dsSplitCoverage()
      coverage.levels <- levels(ds$Coverage)
      coverage <- coverage.levels[round(input$dotplot_hover$x)]
      paste0("You've selected the journalist topic: ", coverage)
    }
  })
  
  # Make dotplot interactive by adding hover feature ------------------------------
  # When hovering over the dotplot, the country will be displayed to user ---------
  output$dotplot.y.info <- renderText({
    if (is.null(input$dotplot_hover)) {
      return("")
    } else {
      ds <- dsSplitCoverage()
      ds$`Country Killed` <- droplevels(ds$`Country Killed`)
      ds$`Country Killed` <- as.factor(str_trim(ds$`Country Killed`))
      country.levels <- levels(ds$`Country Killed`)
      print(country.levels)
      country <- country.levels[round(input$dotplot_hover$y)]
      paste0("You've selected the country: ", country)
    }
  })
  
  # Number of male deaths value box ----------------------------------------------
  # Note: renderUI() used not renderValueBox() because width only works in prior
  output$sex.deaths <- renderUI({
    ds <- deathsSubset()
    # Get the count of deaths for both sexes
    sex.counts <- table(ds$Sex)
    # Extract the count of deaths for males to display in dashboard
    male.count <- sex.counts[names(sex.counts) == "Male"]
    if (length(male.count) == 0) {
      male.count <- 0
    }
    valueBox(value = male.count, subtitle = "Male Deaths", color = "green", width = 3)
  })
  
  # Keep watch on the user changing the fill on the barplot
  # Return which column the user selected
  barplot.fill.choice <- reactive({
    if (is.null(input$fill.choice)) { return ("")}
    if (input$fill.choice == "Tortured") {
      return (deathsSubset()$Tortured)
    } else if (input$fill.choice == "Threatened") {
      return (deathsSubset()$Threatened)
    } else {
      return (deathsSubset()$Freelance)
    }
  })
  
  # Plot the number of deaths per nationality ------------------------------------
  # Call the function to read in user's selection of how to fill the barplot -----
  output$barplot.nationality <- renderPlot({
      ds <- deathsSubset()
      req(nrow(ds) > 2)
      # Find the 10 nationalities with the most deaths to plot on barplot --------
      top.nationalities <- names(tail(sort(table(ds$Nationality)),10))
      ggplot(ds, aes(x = Nationality, fill = barplot.fill.choice())) + geom_bar(color = "black") +
        scale_x_discrete(limits = top.nationalities) + scale_fill_brewer(palette = "Accent") +
        labs(x = "Journalist Nationality", y = "Number of Journalist Deaths",
             title = "Journalist Death by Nationality", fill = input$fill.choice)
      })
  
  # Number of journalists taken captive value box ---------------------------------
  output$captive <- renderValueBox({
    ds <- deathsSubset()
    # Get the count of journalists taken captive and those not taken captive
    captive <- table(str_trim(ds$`Taken Captive`))
    # Extract the count of only those taken captive to display in dashboard
    count.captive <- captive[names(captive) == "Yes"]
    if (length(count.captive) == 0) {
      count.captive <- 0
    }
    valueBox(value = count.captive, subtitle = "Journalists Taken Captive Before Death", color = "purple")
  })
  
  # Plot the number of deaths over the years, grouped by the Source of the Murder
  output$source.by.year <- renderPlot({
    ds <- deathsSubset()
    req(nrow(ds) > 3)
    # Take the filtered data, split on Source of Fire (there are some comma separated values) and create a new dataframe
    ds_split <- setDT(ds)[, strsplit(str_trim(as.character(`Source of Fire`)), ",", fixed=TRUE),
                              by = .(Tortured, `Year of Death`,`Source of Fire`)
                              ][,.(`Source of Fire` = V1, Tortured, `Year of Death`)]
    
    ds_split$`Source of Fire` <- as.factor(str_trim(ds_split$`Source of Fire`))
    
    ggplot(ds_split, aes(x = `Year of Death`)) +
      geom_density(aes(fill=`Source of Fire`), alpha = .4, position = "stack", adjust = input$adjust.choice) + 
      scale_fill_brewer(palette = "Purples") +
      labs(title="Distribution of Deaths over Time", 
           subtitle="Year of Death grouped by Source of Murder",
           x="Year",
           y = "Density",
           fill="Source of Murder")
  })
  
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$deathstable <- renderDataTable({
    datatable(data = deathsSubset(), options = list(orderClasses = TRUE, autoWidth = FALSE, scrollX = TRUE,
                                                     pageLength = 5),
              class = 'cell-border stripe', rownames = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

