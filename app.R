library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(textrecipes)

source(here::here("./helpers.R"))

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
              column(width = 8,
                     h3("Use the charitable object to classify charities"),
                     textAreaInput("charity_object", "Enter charitable object", 
                                   "to promote the relief of physically disabled people in the county of surrey in such ways as the association...",
                                   width = "100%",
                                   rows = 3
                     ),
                     actionButton('classify','Classify', icon = icon('cogs'))
                     ),
              column(br(),
                     width = 8,
                     "Based on its charitable object, the charity's primary activity is:",
                     uiOutput("spinner")
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

    output$value <- renderText({
      a$result <- voting_wf(data.frame(registered_charity_number = 1234,
                                       charitable_objects = input$charity_object))
      # Display the prediction value
      print(a$result$.pred_class)
    })
    
    output$spinner <- renderUI({
      shinycssloaders::withSpinner(textOutput("value"))
    })
    
  })
  

  
  

}


shinyApp(ui, server)