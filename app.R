library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(textrecipes)

source(here::here("./helpers.R"))

## load models
final_trained_models <- list()

for (i in seq(1:4)){
  final_trained_models[[i]] <- readRDS(paste0(here::here(), "/../models", "/model", i, ".rds"))
}

## load accuracy data
acc_per_cat <- read_csv(here::here("./data/acc_per_cat.csv"))

## UI design
ui <- dashboardPage(

  dashboardHeader(title = "Charity Classifier"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("question-circle")),
      menuItem("Classification tool", tabName = "class_tool", icon = icon("search"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              h2("About this tool"),
              fluidRow(
                
                box(
                  title = "What is this tool?",
                  status = "primary", solidHeader = TRUE,
                  width = 12, 
                  "When registering with the Commission, charities must declare their sector of activity.
                  This is a tick box with 16 options, due to the nature of the sector, most organsiation tick at least 2 and some as many as all 16 boxes.
                  This tool classifies organisations based on their charitable object: given the wording of their object, it returns the most
                  likely sector of activity."
                ),
                box(
                  title = "Why does it matter?",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  "The simple question of 'How much charitable expenditure goes towards Education and Training vs. Health' is hard to answer because a lot of charities fall into both categories.
                  This tool helps defining the most likely main charitable activity to avoid double counting."
                ),
                box(
                  title = "What's under the hood?",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  "A machine learning model was trained on charities that have only one sector of activity. This model can read a new
                  charitable object and return the most likely main sector of activity."
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "class_tool",
              fluidRow(
                
                box(
                  title = "Charitable object to classify",
                  status = "primary", solidHeader = TRUE,
                  textAreaInput("charity_object", "Enter charitable object", "This charity helps young adults...", width = "1000px"),
                  actionButton('classify','Classify', icon = icon('cogs'))
                ),
                
                box(title = 'Most likely activity of the organisation:',
                    status = 'success', solidHeader = TRUE,
                    width = 6, 
                    div(h5("Based on its charitable object, the charity's primary activity is:")),
                    verbatimTextOutput("value", placeholder = TRUE)
                )
              )
      )
    )
  )
)

## Server
server <- function(input, output) {
  
  # React value when using the action button
  a <- reactiveValues(result = NULL)
  
  # Generate prediction
  observeEvent(input$classify, {
    a$result <- voting_wf(data.frame(registered_charity_number = 1234,
                          charitable_objects = input$charity_object))
    
  })
  
  output$value <- renderText({
    # Display the prediction value
    print(a$result$.pred_class)
  })
  
}




shinyApp(ui, server)