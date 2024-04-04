#### Preamble ####
# Purpose: Creates the shiny application for holocaust victims by birthplace.
# Author: Raghav Bhatia
# Date: 2 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT

library(shiny)
library(tidyverse)
library(ggplot2)
library(readr)

# Load data
Holocaust_casualties <- read_csv(
  "data/cleaned_data/Auschwitz_Death_Certificates_1942-1943 - Auschwitz.csv")

# Create a table to count victims by nationality
casualty_count <- Holocaust_casualties |>
  group_by(Birthplace) |>
  summarise(Casualties = n()) |>
  filter(Casualties >= 30)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Holocaust Casualties by Nationality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "birthplaces",
          label = "Nationality:",
          choices = casualty_count$Birthplace,
          multiple = TRUE,
          selected = c("Berlin", "Amsterdam", "Hamburg", "Krakau", "Paris")
        )
      ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_nationalities <- reactive({
    casualty_count |>
      filter(Birthplace %in% input$birthplaces)
  })
  
  output$distPlot <- renderPlot({
    # Draw the histogram with the specified number of bins
    selected_nationalities() |>
      ggplot(aes(x = Birthplace, y = Casualties)) +
      geom_col() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      labs(
        x = "Nationality",
        y = "Number of Casualties"
      )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
