#axis handling https://claude.ai/chat/7cf2082f-47be-4243-82b3-256a688dd380 

# Load necessary libraries
rm(list = ls())
library(shiny)
library(tidyr)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(stringr)
library(colourpicker)
library(seasonal)
library(lubridate)
library(plotly)
load("data_index.RData")

# Get current month and year
current_month <- as.integer(format(Sys.Date(), "%m"))
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Define UI
# Define UI
css <- HTML("
/* Style for the smaller control boxes within panels */
.control-box {
  background-color: #f5f5f5 !important;  /* Light grey background */
  border: 1px solid #ddd !important;     /* Light grey border */
  margin: 5px;
  padding: 10px;
}

/* Make the large well panels blue */
.well {
  background-color: #f0f8ff !important;  /* Light blue background */
  border: 1px solid #add8e6 !important;  /* Blue border */
}

/* Override nested wellPanel backgrounds */
.well .well.control-box {
  background-color: #f5f5f5 !important;  /* Ensure control boxes stay light grey */
  border: 1px solid #ddd !important;     /* Light grey border */
}

/* Style for all tabs container */
.nav-tabs {
  border-bottom: 1px solid #ddd;
  display: flex;
  width: 100%;
}

/* Make tabs take equal width */
.nav-tabs > li {
  flex: 1;
  text-align: center;
}

/* Normal state of tabs */
.nav-tabs > li > a {
  background-color: #f0f8ff;  /* Same light blue as panels */
  color: #333;
  border: 1px solid #add8e6;
  margin-right: 2px;
  white-space: normal;  /* Allow text to wrap */
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

/* Hover state of tabs */
.nav-tabs > li > a:hover {
  background-color: #e6f3ff;
  border-color: #add8e6;
}

/* Active/selected tab */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background-color: #4a90e2;  /* Bright blue for active tab */
  color: white;
  border: 1px solid #357abd;
  border-bottom-color: transparent;
}

/* Style for action buttons container */
.control-box .btn-group-vertical {
  display: flex;
  flex-direction: column;
  width: 100%;
  gap: 15px;  /* Increased gap between buttons */
}

/* Style for individual buttons */
.control-box .btn {
  margin-bottom: 10px;  /* Add margin to bottom of each button */
  width: 100%;         /* Make buttons full width */
  white-space: normal; /* Allow text to wrap */
  min-height: 38px;    /* Minimum height for buttons */
}

/* Remove margin from last button */
.control-box .btn:last-child {
  margin-bottom: 0;
}

/* Custom button styling */
.custom-button {
  background-color: #f8f9fa;
  border: 1px solid #ddd;
  transition: background-color 0.3s;
}

.custom-button:hover {
  background-color: #e9ecef;
}

/* Style for stacked date range inputs */
.input-daterange input {
  display: block !important;
  width: 100% !important;
  margin-bottom: 5px;
}

/* Custom flex row for 5 equal columns */
.flex-row-5 {
  display: flex;
  flex-wrap: wrap;
  margin-right: -15px;
  margin-left: -15px;
}

/* Default mobile-first layout - full width columns */
.flex-row-5 > div {
  flex: 0 0 100%;
  max-width: 100%;
  padding-right: 15px;
  padding-left: 15px;
  margin-bottom: 15px;  /* Add space between stacked columns */
}

/* Medium devices (tablets, 768px and up) */
@media (min-width: 768px) {
  .flex-row-5 > div {
    flex: 0 0 50%;
    max-width: 50%;
  }
}

/* Large devices (desktops, 992px and up) */
@media (min-width: 992px) {
  .flex-row-5 > div {
    flex: 0 0 20%;
    max-width: 20%;
    margin-bottom: 0;  /* Remove bottom margin in horizontal layout */
  }
}

/* Ensure control boxes take full height in flex layout */
.flex-row-5 .control-box {
  height: 100%;
}")

ui <- fluidPage(
  tags$head(
    tags$style(css)
  ),
  
  titlePanel("Industrial Strategy Economic Monitor (ISEM) Viewer"),
  
  tabsetPanel(
    # First tab - Labor Market Status
    # First tab - Labor Market Status
    tabPanel("Select",
             fluidRow(
               column(12,
                      wellPanel(
                        div(class = "flex-row-5",
                            # First control box - Date
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Date", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                dateInput("startDate", "Start:",
                                          value = as.Date(paste("1900-01-01")),
                                          min = "1900-01-01",
                                          max = Sys.Date(),
                                          format = "yyyy-mm",
                                          startview = "year"),
                                dateInput("endDate", "End:",
                                          value = Sys.Date(),
                                          min = "1900-01-01",
                                          max = Sys.Date(),
                                          format = "yyyy-mm",
                                          startview = "year")
                              )
                            ),
                            # Second control box - Source
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Source", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                pickerInput("datasetDropdown", "Select Dataset:",
                                            choices = unique(data$Dataset),
                                            options = list(`live-search` = TRUE)),
                                pickerInput("indicatorDropdown", "Select Indicator:",
                                            choices = unique(data$Indicator),
                                            selected = unique(data$Indicator)[1],
                                            options = list(`live-search` = TRUE))
                              )
                            ),
                            # Third control box - Filter
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Filter", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                pickerInput("thematicGroupings", "Industry Constraint",
                                            choices = c("No Constraint", "INFRASTRUCTURE", "Roads and Bridges", "Public Transit",  "EV Infrastructure", "Airports", "Port Infrastructure",
                                                        "Water Infrastructure", "Internet and Communications Infrastructure",
                                                        "Electrical Grid and Transmission Infrastructure", "CLEAN ENERGY", "Wind Turbines",
                                                        "Solar Panels", "Batteries", "Electric Vehicles", "Nuclear Power", "Carbon Capture and Storage",
                                                        "Clean Hydrogen", "Critical Minerals", "Geothermal Energy", "Heat Pumps", "SEMICONDUCTORS", "HOUSING", "AUTOMOBILES"),
                                            options = list(`live-search` = TRUE)),
                                pickerInput("naicsConstraint", "NAICS Code Constraint:",
                                            choices = c("No Constraint", "", unique(data$index_col[grepl("^\\d{3}\\b", data$NAICS_Code)])),
                                            options = list(`live-search` = TRUE))
                              )
                            ),
                            # Fourth control box - Sector
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Sector", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                pickerInput("naicsIndex", "Select NAICS Code:",
                                            choices = NULL,
                                            selected = NULL,
                                            options = list(`live-search` = TRUE)),
                                checkboxInput("showSubIndustries", "Show Sub-Industries?", value = FALSE),
                                checkboxInput("showAllSeries", "Show All Series?", value = FALSE)
                              )
                            ),
                            # Fifth control box - Seasonality (moved from Transform tab)
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Seasonality", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                selectInput("seasonalAdjustment", "Seasonal Adjustment:",
                                            choices = NULL),
                                checkboxInput("useSeasonalAdjustment", "Apply Seasonal Adjustment?", value = FALSE)
                              )
                            )
                        )
                      )
               )
             )
    ),
    
    # Second tab - Transforms (now with Seasonality removed)
    tabPanel("Transform",
             fluidRow(
               column(12,
                      wellPanel(
                        div(class = "flex-row-5",
                            # Index
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Index", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                checkboxInput("useIndexDate", "Index to Date", value = FALSE),
                                dateInput("indexDate", "Index Date:",
                                          value = as.Date("2020-01-01"),
                                          min = "2000-01-01",
                                          max = Sys.Date(),
                                          format = "yyyy-mm",
                                          startview = "year")
                              )
                            ),
                            # Moving Average
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Moving Average", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                checkboxInput("useMovingAverage", "Apply Moving Average?", value = FALSE),
                                numericInput("movingAveragePeriod", "Period (months):", value = 12, min = 1)
                              )
                            ),
                            
                            # Empty div to maintain layout
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Change", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                checkboxInput("useChange", "Apply Change?", value = FALSE),
                                numericInput("changePeriod", "Period (months):", value = 12, min = 1)
                              )
                            ),
                            # Percent Change
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("Percent Change", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                checkboxInput("usePercentChange", "Apply Percent Change?", value = FALSE),
                                numericInput("percentChangePeriod", "Period (months):", value = 12, min = 1)
                              )
                            ),
                            # CAGR
                            div(
                              wellPanel(
                                class = "control-box",
                                h4("CAGR", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                checkboxInput("useCompoundAnnualGrowthRate", "Apply Compound Annual Growth Rate?", value = FALSE),
                                numericInput("compoundAnnualGrowthRatePeriod", "Period (months):", value = 1, min = 1)
                              )
                            )
                            
                        )
                      )
               )
             )
    ),
    
    # Third tab - Manage
    tabPanel("Manage",
             fluidRow(
               column(12,
                      wellPanel(
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   class = "control-box",
                                   h4("Actions", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                   div(
                                     class = "btn-group-vertical",
                                     style = "width: 100%;",
                                     actionButton("addToStoredData", HTML("Add Selected Data<br>To Finished Visualization"),
                                                  class = "btn custom-button"),
                                     downloadButton("downloadStoredData", "Download Data"),
                                     actionButton("resetInputs", "Reset Inputs"),
                                     actionButton("clearStoredData", HTML("Clear Data"),
                                                  class = "btn custom-button")
                                   )
                                 )
                          ),
                          column(9,
                                 wellPanel(
                                   class = "control-box",
                                   h4("Saved Data", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                   uiOutput("seriesManager")
                                 )
                          )
                        )
                      )
               )
             )
    ),
    
    # Fourth tab - Visualize
    tabPanel("Visualize",
             fluidRow(
               column(12,
                      wellPanel(
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   class = "control-box",
                                   h4("Add Graphics", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                   fluidRow(
                                     column(6, checkboxInput("addHorizontalLine", "Add Horizontal Line", value = FALSE)),
                                     column(6, numericInput("horizontalLineValue", "Line Value", value = 0, step = 0.1))
                                   ),
                                   fluidRow(
                                     column(6, checkboxInput("addVerticalLine", "Add Vertical Line", value = FALSE)),
                                     column(6, dateInput("verticalLineDate", "Vertical Line Date:",
                                                         value = Sys.Date(),
                                                         min = "2000-01-01",
                                                         max = Sys.Date(),
                                                         format = "yyyy-mm",
                                                         startview = "year"))
                                   ),
                                   # Add this new row:
                                   fluidRow(
                                     column(12, checkboxInput("addRecessionShading", "Add NBER Recession Shading", value = FALSE))
                                   )
                                 )
                          ),
                          column(4,
                                 wellPanel(
                                   class = "control-box",
                                   h4("Layout", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                   fluidRow(
                                     column(6, numericInput("xLabelFreq", "X-axis Label Frequency (years):", value = 3)),
                                     column(6, numericInput("storedXLabelFreq", "X-axis Label Frequency (years):", value = 3))
                                   )
                                 )
                          ),
                          column(4,
                                 wellPanel(
                                   class = "control-box",
                                   h4("Export", style = "color: #2c3e50; border-bottom: 1px solid #add8e6; padding-bottom: 10px;"),
                                   textInput("finishedVisualizationTitle", "Title:", value = "Finished Visualization")
                                 )
                          )
                        )
                      )
               )
             )
    )
  ),
  
  # Add plots below all tabs
  fluidRow(
    column(12,
           # First chart takes full width
           plotlyOutput("lineChart", height = "600px"),
           # Add some spacing between charts
           tags$div(style = "height: 40px"),
           # Second chart takes full width
           plotlyOutput("storedLineChart", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  recessions_df <- data.frame(
    start = as.Date(c(
      "1973-11-01", "1980-01-01", "1981-07-01", 
      "1990-07-01", "2001-03-01", "2007-12-01", 
      "2020-02-01"
    )),
    end = as.Date(c(
      "1975-03-01", "1980-07-01", "1982-11-01", 
      "1991-03-01", "2001-11-01", "2009-06-01", 
      "2020-04-01"
    ))
  )
  
  # Create a reactive value to store the stored data
  storedData <- reactiveVal(data.frame(Date = as.Date(character()), 
                                       NAICS_Code = character(),
                                       NAICS_Name = character(),
                                       Indicator = character(),
                                       Value = numeric(),
                                       index_col = character(),
                                       Transform_Name = character(),
                                       Combined = character(),
                                       Axis_Type = character()))
  
  
  # Reactive expression to extract the first three characters of the selected NAICS Constraint
  naicsConstraintCode <- reactive({
    if (input$naicsConstraint != "" && input$naicsConstraint != "No Constraint") {
      substr(input$naicsConstraint, 1, 3)
    } else {
      ""
    }
  })
  
  # Thematic NAICS codes
  thematicNAICS <- reactive({
    if (input$thematicGroupings == "No Constraint") {
      unique(data$NAICS_Code)
    } else {
      switch(input$thematicGroupings,
             "Roads and Bridges" = c("32412", "32732", "3311", "3323", "33312", "3273", "3273", "3323", "333"),
             "Public Transit" = c("3261", "3272", "3323", "3323", "332", "3325", "332", "33313", "3334", "3336", "333", "3343", "3344", "3345", "3353", "3359", "3365"),
             "EV Infrastructure" = c("3273", "3315", "332", "3342", "3345", "3353", "33591", "33593", "3359"),
             "Airports" = c("32412", "3261", "32732", "3273", "3273", "3279", "3323", "3323", "3323", "3323"),
             "Port Infrastructure" = c("32732", "3311", "3323", "3323", "33312", "333", "333", "333"),
             "Water Infrastructure" = c("3261", "3273", "331", "3329", "3333", "333", "3345", "3345", "3359"),
             "Internet and Communications Infrastructure" = c("3323", "3342", "3342", "3342", "3344", "3359", "3359"),
             "Electrical Grid and Transmission Infrastructure" = c("3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359"),
             "SEMICONDUCTORS" = c("3329", "3329", "3332", "3332", "3333", "3333", "333", "3344", "3345", "3353", "3391"),
             "Wind Turbines" = c("3315", "3323", "332991", "3336", "3336", "3336", "3344", "3359", "3359"),
             "Solar Panels" = c("32518", "3261", "3272", "3314", "3323", "332", "3332", "3332", "3344", "3344", "33593", "3359"),
             "Batteries" = c("32518", "3261", "33141", "3314", "3344", "3344", "33591"),
             "Electric Vehicles" = c("3342", "3344", "3344", "3345", "3353", "33591", "3363"),
             "Nuclear Power" = c("3323", "3323", "332", "332", "3329", "3336", "3336", "3336", "3345", "3345", "3353", "3359"),
             "Carbon Capture and Storage" = c("32518", "3329", "33313", "3334", "3345", "3345"),
             "Clean Hydrogen" = c("32518", "3329", "33313", "333", "3344", "3345"),
             "Critical Minerals" = c("21221", "21222", "2123", "2122", "32518", "33141", "3314", "33313"),
             "Geothermal Energy" = c("3327", "3329", "33313", "3334", "3345", "3353"),
             "Heat Pumps" = c("3314", "3334", "3345", "3353"),
             "HOUSING" = c("32191", "32191", "3261", "32712", "32731", "32732", "3274", "3279", "3311", "331", "3325", "3334", "3351", "33522", "3371"),
             "AUTOMOBILES" = c("3315", "3315", "3321", "3335", "3336", "3344", "3362", "3363", "3363", "3363", "3363"),
             "INFRASTRUCTURE" = c("32412", "32732", "3311", "3323", "33312", "3273", "3273", "3323", "333",
                                  "3261", "3272", "3323", "3323", "332", "3325", "332", "33313", "3334", "3336", "333", "3343", "3344", "3345", "3353", "3359", "3365",
                                  "3273", "3315", "332", "3342", "3345", "3353", "33591", "33593", "3359",
                                  "32412", "3261", "32732", "3273", "3273", "3279", "3323", "3323", "3323", "3323",
                                  "32732", "3311", "3323", "3323", "33312", "333", "333", "333",
                                  "3261", "3273", "331", "3329", "3333", "333", "3345", "3345", "3359",
                                  "3323", "3342", "3342", "3342", "3344", "3359", "3359",
                                  "3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359"),
             "CLEAN ENERGY" = c("3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359",
                                "3315", "3323", "332991", "3336", "3336", "3336", "3344", "3359", "3359",
                                "32518", "3261", "3272", "3314", "3323", "332", "3332", "3332", "3344", "3344", "33593", "3359",
                                "32518", "3261", "33141", "3314", "3344", "3344", "33591",
                                "3342", "3344", "3344", "3345", "3353", "33591", "3363",
                                "3323", "3323", "332", "332", "3329", "3336", "3336", "3336", "3345", "3345", "3353", "3359",
                                "32518", "3329", "33313", "3334", "3345", "3345",
                                "32518", "3329", "33313", "333", "3344", "3345",
                                "21221", "21222", "2123", "2122", "32518", "33141", "3314", "33313",
                                "3327", "3329", "33313", "3334", "3345", "3353",
                                "3314", "3334", "3345", "3353")
      )
    }
  })
  
  #Observers to make sure NAICS Constraint and Thematic Groupings are mutually exclusive
  observeEvent(input$naicsConstraint, {
    if (input$naicsConstraint != "No Constraint") {
      updateSelectInput(session, "thematicGroupings", selected = "No Constraint")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$thematicGroupings, {
    if (input$thematicGroupings != "No Constraint") {
      updateSelectInput(session, "naicsConstraint", selected = "No Constraint")
    }
  }, ignoreInit = TRUE)
  
  
  # Filtered NAICS choices based on Thematic Constraints, NAICS Constraint, and selected Indicator
  filteredNAICS <- reactive({
    if (input$thematicGroupings != "No Constraint") {
      data %>%
        filter(NAICS_Code %in% thematicNAICS(),
               Indicator == input$indicatorDropdown)
    } else if (naicsConstraintCode() != "") {
      data %>%
        filter(str_starts(NAICS_Code, naicsConstraintCode()),
               Indicator == input$indicatorDropdown)
    } else {
      data %>%
        filter(Indicator == input$indicatorDropdown)
    }
  })
  
  #Observer to run the dataset/indicator dropdown
  observeEvent(input$datasetDropdown, {
    # Filter indicators based on selected dataset
    filtered_indicators <- data %>%
      filter(Dataset == input$datasetDropdown) %>%
      pull(Indicator) %>%
      unique()
    
    # Update the indicator dropdown using picker input
    updatePickerInput(session, "indicatorDropdown",
                      choices = filtered_indicators,
                      selected = filtered_indicators[1])
  })
  
  observe({
    req(input$datasetDropdown, input$indicatorDropdown)
    
    # Get available seasonal adjustment options for current dataset and indicator
    available_adjustments <- data %>%
      filter(Dataset == input$datasetDropdown,
             Indicator == input$indicatorDropdown) %>%
      pull(seasonal_adjustment) %>%
      unique()
    
    # Create named vector for choices
    adjustment_choices <- setNames(
      as.character(available_adjustments),
      ifelse(available_adjustments, "Seasonally Adjusted", "Not Seasonally Adjusted")
    )
    
    # Update the seasonal adjustment dropdown
    updateSelectInput(session, "seasonalAdjustment",
                      choices = adjustment_choices,
                      selected = if(length(available_adjustments) > 0) as.character(available_adjustments[1]))
  })
  observeEvent(c(naicsConstraintCode(), input$indicatorDropdown, input$thematicGroupings), {
    updatePickerInput(session, "naicsIndex",
                      choices = unique(filteredNAICS()$index_col),
                      selected = unique(filteredNAICS()$index_col)[1])
  })
  
  #  Define reactive variable for transform name
  transformName <- reactive({
    transforms <- c()
    if (input$useMovingAverage == TRUE) {
      transforms <- c(transforms, paste("Moving Average (", input$movingAveragePeriod, " months)", sep = ""))
    }
    if (input$usePercentChange == TRUE) {
      transforms <- c(transforms, paste("Percent Change (", input$percentChangePeriod, " months)", sep = ""))
    }
    if (input$useCompoundAnnualGrowthRate == TRUE) {
      transforms <- c(transforms, paste("Compound Annual Growth Rate (", input$compoundAnnualGrowthRatePeriod, " months)", sep = ""))
    }
    if (input$useChange == TRUE) {
      transforms <- c(transforms, paste("Change (", input$changePeriod, " months)", sep = ""))
    }
    if (length(transforms) == 0) {
      transforms <- ""
    }
    transforms
  })
  
  #  Create a list of NAICS codes based on the selected index column, Show Sub-Industries option, and Show All Included Series option
  selectedNAICS <- reactive({
    # Now using boolean values from checkboxes instead of "Yes"/"No" strings
    if (input$showAllSeries) {
      unique(filteredNAICS()$index_col)
    } else if (input$showSubIndustries) {
      selected_index <- input$naicsIndex
      selected_code <- strsplit(selected_index, " - ")[[1]][1]
      naics_codes <- c(selected_code, paste0(selected_code, 1:9))
      data %>%
        filter(NAICS_Code %in% naics_codes) %>%
        pull(index_col)
    } else {
      input$naicsIndex
    }
  })
  
  #  Convert selected months and years to dates
  startDate <- reactive({
    # Force day to first of month
    as.Date(format(input$startDate, "%Y-%m-01"))
  })
  
  endDate <- reactive({
    # Force day to first of month
    as.Date(format(input$endDate, "%Y-%m-01"))
  })
  
  #  Convert selected index month and year to date
  indexDate <- reactive({
    # Force day to first of month
    as.Date(format(input$indexDate, "%Y-%m-01"))
  })
  
  #fiilter data based on user selections
  filteredData <- reactive({
    filtered <- data %>%
      filter(Date >= startDate() & Date <= endDate() &
               index_col %in% selectedNAICS() &
               Indicator == input$indicatorDropdown &
               Dataset == input$datasetDropdown &
               seasonal_adjustment == as.logical(input$seasonalAdjustment)) %>%
      arrange(Date) %>%
      group_by(Indicator, index_col) %>%
      arrange(Date, .by_group = TRUE)
    
    # Apply indexing if useIndexDate is TRUE
    if (input$useIndexDate) {
      index_date <- paste(month.name[as.integer(format(indexDate(), "%m"))], 
                          format(indexDate(), "%Y"))
      filtered <- filtered %>%
        group_by(Indicator, index_col) %>%
        mutate(Value = Value / Value[Date == indexDate()][1] * 100,
               Transform_Name = paste("Indexed to", index_date)) %>%
        ungroup()
    } else {
      filtered <- filtered %>%
        mutate(Transform_Name = "")
    }
    
    # Add Axis Type column using boolean checkbox values
    filtered <- filtered %>%
      mutate(Axis_Type = ifelse(input$usePercentChange || 
                                  input$useCompoundAnnualGrowthRate, 
                                "Percentage", "Index"))
    
    # Apply X-13 seasonal adjustment if selected
    if (input$useSeasonalAdjustment) {
      filtered <- filtered %>%
        group_modify(~{
          # Create a copy of the input data frame
          result_df <- .x %>% arrange(Date)
          
          # Convert to time series object
          ts_data <- ts(result_df$Value, 
                        start = c(year(min(result_df$Date)), month(min(result_df$Date))),
                        frequency = 12)
          
          # Apply X-13-ARIMA-SEATS
          tryCatch({
            seas_adj <- seas(ts_data, 
                             transform.function = "none",
                             regression.aictest = NULL,
                             outlier = NULL,
                             slidingspans = NULL,
                             history = NULL)
            
            # Extract seasonally adjusted series and update values
            result_df$Value <- as.numeric(final(seas_adj))
            result_df$Transform_Name <- if(result_df$Transform_Name[1] == "") {
              "X-13 Seasonally Adjusted"
            } else {
              paste(result_df$Transform_Name, "X-13 Seasonally Adjusted", sep = ", ")
            }
            
          }, error = function(e) {
            warning(paste("Seasonal adjustment failed for", unique(.x$index_col), 
                          ". Returning original data."))
          })
          
          result_df
        })
    }
    
    # Apply other transforms in the specified order
    transforms <- transformName()
    for (transform in transforms) {
      if (grepl("Moving Average", transform)) {
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = dplyr::lag(zoo::rollmean(Value, k = input$movingAveragePeriod, 
                                                  fill = NA, align = "right"), 
                                    input$movingAveragePeriod - 1),
                 Transform_Name = paste(Transform_Name, transform, sep = ", "))
      } else if (grepl("Percent Change", transform)) {
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = (Value - lag(Value, n = input$percentChangePeriod)) / 
                   lag(Value, n = input$percentChangePeriod),
                 Transform_Name = paste(Transform_Name, transform, sep = ", "))
      } else if (grepl("Compound Annual Growth Rate", transform)) {
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = ((Value / lag(Value, n = input$compoundAnnualGrowthRatePeriod)) ^ 
                            (1 / (input$compoundAnnualGrowthRatePeriod / 12)) - 1),
                 Transform_Name = paste(Transform_Name, transform, sep = ", "))
      } else if (grepl("Change", transform)) {
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = Value - lag(Value, n = input$changePeriod),
                 Transform_Name = paste(Transform_Name, transform, sep = ", "))
      }
    }
    
    # Final cleanup and ordering
    filtered <- filtered %>%
      tidyr::drop_na() %>%
      ungroup() %>%
      arrange(Date, Indicator, index_col)
    
    filtered
  })
  
  #  Update radio buttons based on user selection
  observeEvent(input$usePercentChange, {
    if (input$usePercentChange) {
      updateCheckboxInput(session, "useCompoundAnnualGrowthRate", value = FALSE)
    }
  })
  
  observeEvent(input$useCompoundAnnualGrowthRate, {
    if (input$useCompoundAnnualGrowthRate) {
      updateCheckboxInput(session, "usePercentChange", value = FALSE)
    }
  })
  
  #  Render line chart for filtered data
  output$lineChart <- renderPlotly({
    # Initialize the plot with hover formatting
    p <- plot_ly(filteredData(), x = ~Date,
                 hovertemplate = paste(
                   "<b>%{text}</b><br>",
                   "<span style='color:%{line.color}'></span> Value: %{y:.2f}",
                   "<extra></extra>"
                 ),
                 text = ~index_col) %>%
      layout(
        hoverlabel = list(
          align = "left",
          bgcolor = "white",
          bordercolor = "darkgray"
        ),
        hovermode = "x unified"
      )
    
    # Handle recession shading
    recession_shapes <- list()
    if (input$addRecessionShading) {
      # Get the date range of our displayed data
      data_start <- min(filteredData()$Date)
      data_end <- max(filteredData()$Date)
      
      # Filter recessions to only show those overlapping with our data
      visible_recessions <- recessions_df %>%
        filter(
          (start >= data_start & start <= data_end) |
            (end >= data_start & end <= data_end) |
            (start <= data_start & end >= data_end)
        ) %>%
        mutate(
          # Clip recession dates to our data range
          start = pmax(start, data_start),
          end = pmin(end, data_end)
        )
      
      # Create shapes for each visible recession period
      recession_shapes <- lapply(seq_len(nrow(visible_recessions)), function(i) {
        list(
          type = "rect",
          fillcolor = "rgb(200, 200, 200)",
          opacity = 0.5,
          line = list(width = 0),
          x0 = visible_recessions$start[i],
          x1 = visible_recessions$end[i],
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          layer = "below"
        )
      })
    }
    
    # Add each series as a line trace
    for (series in unique(paste(filteredData()$Indicator, filteredData()$Transform_Name, filteredData()$index_col, sep = " - "))) {
      series_data <- filteredData() %>% 
        filter(paste(Indicator, Transform_Name, index_col, sep = " - ") == series)
      
      p <- p %>% add_lines(
        y = ~Value,
        data = series_data,
        name = series,
        line = list(width = 1),
        hoverinfo = "none",
        showlegend = TRUE
      )
    }
    
    # Apply appropriate tick formatting
    if (any(filteredData()$Axis_Type == "Percentage")) {
      p <- p %>% layout(yaxis = list(tickformat = ".1%"))
    } else {
      p <- p %>% layout(yaxis = list(tickformat = ","))
    }
    
    # Add final layout properties
    p %>% layout(
      shapes = recession_shapes,  # Place shapes first in layout
      title = list(text = "Data Finder", font = list(size = 24)),
      xaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Date",
        tickfont = list(size = 12),
        dtick = paste0("M", input$xLabelFreq * 12),
        rangeslider = list(
          visible = TRUE,
          thickness = 0.1,
          bgcolor = "#F3F3F3",
          yaxis = list(
            rangemode = "auto",
            fixedrange = FALSE
          )
        )
      ),
      yaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Value", 
        tickfont = list(size = 12),
        fixedrange = FALSE,
        autorange = TRUE,
        rangemode = "auto"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5,
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1
      ),
      margin = list(
        t = 50,
        r = 50,
        b = 200,
        l = 50
      ),
      hoverlabel = list(
        align = "left",
        bgcolor = "white",
        bordercolor = "darkgray"
      ),
      hovermode = "x unified",
      plot_bgcolor = "rgba(255, 255, 255, 0.9)",
      paper_bgcolor = "rgba(255, 255, 255, 0.9)"
    )
  })
  
  # Reactive expression for the chart title
  chartTitle <- reactive({
    if(input$finishedVisualizationTitle == "") {
      "Finished Visualization"
    } else {
      input$finishedVisualizationTitle
    }
  })
  
  # Modify the existing storedLineChart output
  # Render the plotly visualization for stored data with custom colors
  # Render the plotly visualization for stored data with custom colors
  # Render the plotly visualization for stored data with custom colors
  # Render the plotly visualization for stored data with custom colors
  # Render the plotly visualization for stored data with custom colors
  output$storedLineChart <- renderPlotly({
    # Initialize the base plot with proper hover label formatting
    p <- plot_ly(storedData(), x = ~Date) %>%
      layout(
        # Configure right-side y-axis
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = "Right Axis",
          showgrid = FALSE,
          autorange = TRUE,
          fixedrange = FALSE,
          rangemode = "auto"
        ),
        # Set up hover label formatting
        hoverlabel = list(
          align = "left",
          bgcolor = "white",
          bordercolor = "darkgray",
          font = list(family = "Arial", size = 12),
          namelength = -1
        ),
        hovermode = "x unified"
      )
    
    # Handle recession shading if enabled
    recession_shapes <- list()
    if (input$addRecessionShading) {
      # Get the date range of displayed data
      data_start <- min(storedData()$Date)
      data_end <- max(storedData()$Date)
      
      # Filter recessions to only show those overlapping with our data
      visible_recessions <- recessions_df %>%
        filter(
          (start >= data_start & start <= data_end) |
            (end >= data_start & end <= data_end) |
            (start <= data_start & end >= data_end)
        ) %>%
        mutate(
          # Clip recession dates to our data range
          start = pmax(start, data_start),
          end = pmin(end, data_end)
        )
      
      # Create shapes for each visible recession period
      recession_shapes <- lapply(seq_len(nrow(visible_recessions)), function(i) {
        list(
          type = "rect",
          fillcolor = "rgb(200, 200, 200)",
          opacity = 0.5,
          line = list(width = 0),
          x0 = visible_recessions$start[i],
          x1 = visible_recessions$end[i],
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          layer = "below"
        )
      })
    }
    
    # Add each stored series with custom colors
    for (series in unique(storedData()$Combined)) {
      # Filter data for current series
      series_data <- storedData() %>% filter(Combined == series)
      
      # Get line width based on highlight status
      line_width <- if (isTRUE(input[[paste0("highlight_", gsub("[^[:alnum:]]", "", series))]])) 2 else 1
      
      # Check if series should use right axis
      use_right_axis <- isTRUE(input[[paste0("rightaxis_", gsub("[^[:alnum:]]", "", series))]])
      
      # Get the color from the color picker and ensure it exists
      color_input_id <- paste0("color_", gsub("[^[:alnum:]]", "", series))
      series_color <- if (!is.null(input[[color_input_id]])) {
        input[[color_input_id]]
      } else {
        # Fallback to a default color if the input doesn't exist yet
        "#000000"
      }
      
      # Add the line trace for this series
      p <- p %>% add_trace(
        data = series_data,
        x = ~Date,
        y = ~Value,
        name = series,
        type = 'scatter',
        mode = 'lines',
        yaxis = if(use_right_axis) "y2" else "y",
        line = list(
          width = line_width,
          color = series_color
        ),
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "<span style='color:", series_color, "'></span> Value: %{y:.2f}",
          "<extra></extra>"
        ),
        text = ~Combined,
        showlegend = TRUE
      )
    }
    
    # Apply appropriate tick formatting based on axis type
    if (any(storedData()$Axis_Type == "Percentage")) {
      p <- p %>% layout(
        yaxis = list(tickformat = ".1%", autorange = TRUE, fixedrange = FALSE, rangemode = "auto"),
        yaxis2 = list(tickformat = ".1%", autorange = TRUE, fixedrange = FALSE, rangemode = "auto")
      )
    } else {
      p <- p %>% layout(
        yaxis = list(tickformat = ",", autorange = TRUE, fixedrange = FALSE, rangemode = "auto"),
        yaxis2 = list(tickformat = ",", autorange = TRUE, fixedrange = FALSE, rangemode = "auto")
      )
    }
    
    # Add reference lines if enabled
    if (input$addHorizontalLine) {
      p <- p %>% add_segments(
        x = min(storedData()$Date),
        xend = max(storedData()$Date),
        y = input$horizontalLineValue,
        yend = input$horizontalLineValue,
        line = list(color = "black", width = 1),
        showlegend = FALSE,
        name = "Reference Line",
        hoverinfo = "none"
      )
    }
    
    if (input$addVerticalLine) {
      vertical_line_date <- as.Date(format(input$verticalLineDate, "%Y-%m-01"))
      p <- p %>% add_segments(
        x = vertical_line_date,
        xend = vertical_line_date,
        y = min(storedData()$Value),
        yend = max(storedData()$Value),
        line = list(color = "black", width = 1),
        showlegend = FALSE,
        name = "Reference Date",
        hoverinfo = "none"
      )
    }
    
    # Add final layout configuration
    p %>% layout(
      shapes = recession_shapes,  # Add recession shading if enabled
      title = list(text = chartTitle(), font = list(size = 24)),
      xaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Date",
        tickfont = list(size = 12),
        dtick = paste0("M", input$storedXLabelFreq * 12),
        rangeslider = list(
          visible = TRUE,
          thickness = 0.1,
          bgcolor = "#F3F3F3",
          yaxis = list(
            rangemode = "auto",
            fixedrange = FALSE
          )
        )
      ),
      yaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Left Axis", 
        tickfont = list(size = 12),
        autorange = TRUE,
        fixedrange = FALSE,
        rangemode = "auto"
      ),
      yaxis2 = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Right Axis", 
        tickfont = list(size = 12),
        autorange = TRUE,
        fixedrange = FALSE,
        rangemode = "auto"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5,
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1
      ),
      margin = list(
        t = 50,
        r = 50,
        b = 200,
        l = 50
      )
    )
  })  

  #  Add currently visualized data to stored data
  observeEvent(input$addToStoredData, {
    new_data <- filteredData() %>%
      mutate(Combined = paste(Indicator, Transform_Name, index_col, sep = " - "))
    
    # Check if any of the new series already exist in storedData
    existing_series <- unique(storedData()$Combined)
    new_series <- unique(new_data$Combined)
    
    # Only add series that don't already exist
    series_to_add <- new_data %>%
      filter(!(Combined %in% existing_series))
    
    if (nrow(series_to_add) > 0) {
      storedData(rbind(storedData(), series_to_add))
    }
  })
  
  observe({
    # Get current series names
    current_series <- unique(storedData()$Combined)
    
    # Clean up any orphaned UI elements
    all_inputs <- names(input)
    highlight_inputs <- all_inputs[grep("^highlight_", all_inputs)]
    
    for (highlight_input in highlight_inputs) {
      series_name <- gsub("^highlight_", "", highlight_input)
      series_name <- gsub("[^[:alnum:]]", "", series_name)
      
      if (!any(grepl(series_name, gsub("[^[:alnum:]]", "", current_series)))) {
        removeUI(
          selector = paste0("#", highlight_input, "-label"),
          immediate = TRUE
        )
      }
    }
  })
  
  #  Clear stored data
  observeEvent(input$clearStoredData, {
    storedData(data.frame(Date = as.Date(character()),
                          NAICS_Code = character(),
                          NAICS_Name = character(),
                          Indicator = character(),
                          Value = numeric(),
                          index_col = character(),
                          Transform_Name = character(),
                          Combined = character(),
                          Axis_Type = character()))
  })
  
  #  Download handler for stored data
  output$downloadStoredData <- downloadHandler(
    filename = function() {
      paste("final_visualization_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ungroup the data first
      data_to_export <- storedData() %>%
        ungroup() %>%
        # Create a unique identifier for each series
        mutate(Series = paste(Indicator, Transform_Name, index_col, sep = " - ")) %>%
        # Select only the columns we need
        select(Date, Series, Value)
      
      # Pivot the data from long to wide format
      wide_data <- data_to_export %>%
        pivot_wider(names_from = Series, values_from = Value)
      
      # Add the horizontal line value if it's enabled
      if(input$addHorizontalLine == TRUE) {
        wide_data$`User Horizontal Line` <- input$horizontalLineValue
      }
      
      # Add the vertical line date if it's enabled
      if(input$addVerticalLine == TRUE) {
        vertical_line_date <- as.Date(format(input$verticalLineDate, "%Y-%m-01"))
        wide_data$`User Vertical Line` <- format(vertical_line_date, "%Y-%m-%d")
      }
      
      # Write the wide-format data to a CSV file
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  #  Render series manager UI
  # Render the UI for managing stored series, including color pickers
  output$seriesManager <- renderUI({
    series_names <- unique(storedData()$Combined)
    
    # Define a colorblind-friendly default color palette
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                        "#FFFF33", "#A65628", "#F781BF", "#999999")
    
    tagList(
      tags$div(
        lapply(seq_along(series_names), function(i) {
          name <- series_names[i]
          # Calculate default color index, cycling through the palette if needed
          color_index <- ((i-1) %% length(default_colors)) + 1
          
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            tags$span(style = "width: 30%; word-wrap: break-word;", name),
            tags$div(style = "width: 15%; text-align: center;",
                     checkboxInput(paste0("highlight_", gsub("[^[:alnum:]]", "", name)), 
                                   "Bold", value = FALSE)
            ),
            tags$div(style = "width: 15%; text-align: center;",
                     checkboxInput(paste0("rightaxis_", gsub("[^[:alnum:]]", "", name)), 
                                   "Right Axis", value = FALSE)
            ),
            tags$div(style = "width: 15%; text-align: center;",
                     colourInput(paste0("color_", gsub("[^[:alnum:]]", "", name)),
                                 NULL,  # No label
                                 default_colors[color_index])
            ),
            actionButton(paste0("remove_", name), "Remove",  
                         style = "width: 20%; font-size: 0.8em;")
          )
        })
      )
    )
  })
  
  #  Remove series from stored data
  observe({
    series_names <- unique(storedData()$Combined)
    lapply(series_names, function(name) {
      observeEvent(input[[paste0("remove_", name)]], {
        # Create a new data frame excluding the removed series
        new_stored_data <- storedData() %>% 
          filter(Combined != name)
        
        # Update storedData with the new data frame
        storedData(new_stored_data)
        
        # Remove the associated input elements
        removeUI(
          selector = paste0("#highlight_", gsub("[^[:alnum:]]", "", name), "-label"),
          immediate = TRUE
        )
      }, ignoreInit = TRUE)
    })
  })
  
  #  Text output
  output$indicatorDescription <- renderText({
    if (input$indicatorDropdown == "Industrial Production") {
      "The G.17 Industrial Production and Capacity Utilization report is a monthly publication by the Federal Reserve Board that produces an index designed to measure the real output of the manufacturing, mining, and electric and gas utilities industries in the United States. It is released monthly at https://www.federalreserve.gov/releases/g17/default.htm . This data is useful for understanding the level and composition of domestic industrial production in the United States. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (input$indicatorDropdown == "Producer Price Index") {
      "The Producer Price Index statistics are designed to measure changes in the prices paid for upstream inputs by domestic firms. They are produced monthly by the Bureau of Labor Statistics and can be found at: https://www.bls.gov/mxp/ . This data is useful for understanding changes in the price of input costs for domestic producers. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (input$indicatorDropdown == "Import Price Index") {
      "The Import Price Index statistics are designed to measure changes in the prices paid for imports by category of good. They are produced monthly by the Bureau of Labor Statistics and can be found at: https://www.bls.gov/mxp/ . This data is useful for understanding changes in the price of foreign-produced goods. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (input$indicatorDropdown == "Nominal Imports") {
      "This data is an index of the level of nominal imports, where measures of dollar values are produced by the Census Bureau. New releases can be found monthly at https://usatrade.census.gov/ . This data is useful for understanding changes in the total amount paid from month to month for imports of specific goods. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (input$indicatorDropdown == "IPI-Adjusted Imports") {
      "This data is compiled specifically for this viewer, using the Census Bureau's data on nominal imports and the Bureau of Labor Statistics data on Import Prices. The index of Nominal Imports is deflated by the index for Import Prices to create an inflation-adjusted index of imports. This index is more accurate than the index of PPI-Adjusted Imports, but at the cost of more limited data coverage. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (input$indicatorDropdown == "PPI-Adjusted Imports") {
      "This data is compiled specifically for this viewer, using the Census Bureau's data on nominal imports and the Bureau of Labor Statistics data on US Domestic Producer Prices. The index of Nominal Imports is deflated by the index for domestic Producer Prices to create an inflation-adjusted index of imports. This index is less accurate than the index of IPI-Adjusted Imports, but has the benefit of broader data coverage. All data are reported as indices initially benchmarked to January 1 2020."
    } else {
      "No description available for the selected indicator."
    }
  })
  
  #  Reset inputs to default values
  observeEvent(input$resetInputs, {
    updateDateInput(session, "startDate", value = as.Date("2000-01-01"))
    updateDateInput(session, "endDate", value = Sys.Date())
    updateDateInput(session, "indexDate", value = as.Date("2020-01-01"))
    updatePickerInput(session, "thematicGroupings", selected = "No Constraint")
    updatePickerInput(session, "naicsConstraint", selected = "")
    updatePickerInput(session, "naicsIndex", choices = unique(filteredNAICS()$index_col), 
                      selected = unique(filteredNAICS()$index_col)[1])
    updatePickerInput(session, "datasetDropdown", selected = unique(data$Dataset)[1])
    updatePickerInput(session, "indicatorDropdown", selected = unique(data$Indicator)[1])
    updateCheckboxInput(session, "showSubIndustries", value = FALSE)
    updateCheckboxInput(session, "showAllSeries", value = FALSE)
    updateCheckboxInput(session, "useIndexDate", value = FALSE)
    updateCheckboxInput(session, "useMovingAverage", value = FALSE)
    updateNumericInput(session, "movingAveragePeriod", value = 1)
    updateCheckboxInput(session, "usePercentChange", value = FALSE)
    updateNumericInput(session, "percentChangePeriod", value = 3)
    updateCheckboxInput(session, "useChange", value = FALSE)
    updateNumericInput(session, "changePeriod", value = 12)
    updateCheckboxInput(session, "useCompoundAnnualGrowthRate", value = FALSE)
    updateNumericInput(session, "compoundAnnualGrowthRatePeriod", value = 12)
    updateNumericInput(session, "xLabelFreq", value = 3)
    updateNumericInput(session, "storedXLabelFreq", value = 3)
  })
}

#Run the application
shinyApp(ui = ui, server = server)