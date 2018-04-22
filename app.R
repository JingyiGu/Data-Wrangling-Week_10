# Library Required Packages
library(shiny)
library(tidyverse)
library(tidycensus)

#Get data from API
source("api.key.R")
census_api_key(api.key)

# Define UI for app
ui <- fluidPage(
  titlePanel("American Community Survey"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("State", "State",
                  choices = state.abb),
      
      radioButtons("Type", "Type",
                   choices = list("median_gross_rent",
                                  "median_household_income",
                                  "ratio"))
    ),
    
    mainPanel(plotOutput("Plot"))
  )
)

# Define server for app
server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(median_gross_rent = "B25064_001" , median_household_income = "B19013_001"),
      state = input$State,
      geometry = TRUE
    ) %>% .[, -5] %>% data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  output$Plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type) + 
      scale_fill_gradientn(colours = rainbow(7))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
