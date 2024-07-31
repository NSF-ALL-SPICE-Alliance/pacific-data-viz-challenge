library(ggplot2)
library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(fmsb)

data <- read_csv(here("data/Professional Fishing South Province.csv"))


sidebar_content <- selectInput("vessel_id", "Select a Vessel:", choices = unique(data$navire_id))
  
  
  
ui <- page_sidebar(
  
  theme = bs_theme(bootswatch = "darkly"),
  
  title = "Pacific Data Viz: New Caledonia Fishing Dashboard",
  
  sidebar = sidebar(
    sidebar_content,
    
  ), 
  
  layout_columns(
    
    card(card_header(showcase = bs_icon("coin")),
         plotOutput("barPlot")),
    
    card(card_header(showcase = bs_icon("coin")),
         plotOutput("radarPlot")),
    
    col_widths = c(12, 12),
  )
)

server <- function(input, output) {
  selected_vessel_data <- reactive({
    data %>%
      filter(navire_id == input$vessel_id) %>%
      summarize(
        total_profit = sum(carte_benefice, na.rm = TRUE),
        total_expense = sum(carte_depense, na.rm = TRUE),
        total_revenue = sum(carte_recette, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(total_profit, total_expense, total_revenue), 
                   names_to = "metric", 
                   values_to = "value")
  })
  
  output$barPlot <- renderPlot({
    ggplot(selected_vessel_data(), aes(x = metric, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Profit, Expense, and Revenue for Vessel:", input$vessel_id),
        x = "Metric",
        y = "Amount",
        fill = "Metric"
      ) +
      theme_minimal()
  })
  
  output$radarPlot <- renderPlot({
    
    radarplt_data <- data %>% 
      select(carte_benefice, carte_depense, carte_recette, carte_rendement, carte_carburant_qte, carte_tonnage_epe, campagne_benefice, campagne_recette)
    
    
    radarchart(radarplt_data,
               axistype = 1,
               pcol = rgb(0.2, 0.5, 0.5, 0.9),
               pfcol = rgb(0.2, 0.5, 0.5, 0.5),
               plwd = 4,
               cglcol = "grey",
               cglty = 1,
               axislabcol = "grey",
               caxislabels = seq(0, max(radarplt_data), length.out = 5),
               cglwd = 0.8,
               vlcex = 0.8
    )
  })
}

shinyApp(ui = ui, server = server)
