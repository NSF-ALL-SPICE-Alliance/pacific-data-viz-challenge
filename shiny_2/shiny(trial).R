library(tidyverse)
library(here)
library(plotly)
library(shiny)
library(dplyr)
library(sunburstR)
library(stringr)
library(shinythemes)
library(viridis)

nc_cancer_data <- read_csv(here("data/cas_cancer_nc.csv"))

nc_cancer_data <- nc_cancer_data %>%
  mutate(category = case_when(
    `ICD-10` %in% c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14") ~ "Néoplasmes malins des lèvres, de la cavité buccale et du pharynx",
    `ICD-10` %in% c("C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26") ~ "Néoplasmes malins des organes digestifs",
    `ICD-10` %in% c("C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39") ~ "Néoplasmes malins des organes respiratoires et intrathoraciques",
    `ICD-10` %in% c("C40", "C41") ~ "Néoplasmes malins des os et du cartilage articulaire",
    `ICD-10` %in% c("C43", "C44") ~ "Mélanome et autres néoplasmes malins de la peau",
    `ICD-10` %in% c("C45") ~ "Néoplasmes mésothéliaux et des tissus mous",
    `ICD-10` %in% c("C46") ~ "Sarcome de Kaposi",
    `ICD-10` %in% c("C47", "C48", "C49") ~ "Néoplasmes malins des tissus mésothéliaux et mous",
    `ICD-10` %in% c("C50") ~ "Néoplasmes malins du sein",
    `ICD-10` %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58") ~ "Néoplasmes malins des organes génitaux féminins",
    `ICD-10` %in% c("C60", "C61", "C62", "C63") ~ "Néoplasmes malins des organes génitaux masculins",
    `ICD-10` %in% c("C64", "C65", "C66", "C67", "C68") ~ "Néoplasmes malins des voies urinaires",
    `ICD-10` %in% c("C69", "C70", "C71", "C72") ~ "Néoplasmes malins de l'œil, du cerveau et d'autres parties du système nerveux central",
    `ICD-10` %in% c("C73", "C74", "C75") ~ "Néoplasmes malins des glandes endocrines",
    `ICD-10` %in% c("C76", "C77", "C78", "C79", "C80") ~ "Néoplasmes malins des sites mal définis, secondaires et non précisés",
    `ICD-10` %in% c("C81", "C82", "C83", "C84", "C85", "C86", "C88") ~ "Néoplasmes malins des tissus lymphoïdes, hématopoïétiques et apparentés",
    `ICD-10` %in% c("C90", "C91", "C92", "C93", "C94", "C95", "C96") ~ "Néoplasmes malins des tissus lymphoïdes, hématopoïétiques et apparentés",
    TRUE ~ "Autre"
  ))

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(
    div(
      tags$img(src = "SpiceLogo1.png", height = "50px", width = "auto"),
      "Les cas des cancers diagnostiqués en Nouvelle-Calédonie"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Sélectionner le sexe:", choices = c("toutes", unique(nc_cancer_data$sex)), width = '40%'),
      selectInput("age_class", "sélectionner l'âge:", choices = c("toutes", unique(nc_cancer_data$age_class)), width = '80%')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("graphique rayons de soleil", sunburstOutput("sunburstPlot")),
        tabPanel("parcelle de pétales", plotlyOutput("polarPlot")),
        tabPanel("Graphique GGplot", plotOutput("ggplotPlot"))  # New tab for ggplot
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- nc_cancer_data
    if (input$sex != "toutes") {
      data <- data %>% filter(sex == input$sex)
    }
    if (input$age_class != "toutes") {
      data <- data %>% filter(age_class == input$age_class)
    }
    data
  })
  
  sunburst_data <- reactive({
    data <- filtered_data() %>%
      group_by(long_label_disease, category) %>%
      count(name = "total_cases") %>%
      mutate(long_label_disease = str_replace(long_label_disease, 
                                              "Tumeur maligne de la jonction recto-sigmoïdienne", 
                                              "Tumeur maligne de la jonction rectosigmoïdienne")) %>%
      mutate(path = paste(category, long_label_disease, sep = "-")) %>%
      ungroup() %>%
      select(path, total_cases)
    data
  })
  
  output$sunburstPlot <- renderSunburst({
    sunburst(sunburst_data(), legend = FALSE, count = TRUE)
  })
  
  nc_cancer_data_petal <- reactive({
    data <- filtered_data() %>%
      group_by(age_class, category) %>%
      count()
    
    # Reorder the age_class factor levels
    data$age_class <- factor(
      data$age_class,
      levels = c("moins de 20 ans", "20-29 ans", "30-39 ans", "40-49 ans", "50-59 ans", "60-69 ans", "70-79 ans", "80 ans et +")
    )
    
    data
  })
  
  output$polarPlot <- renderPlotly({
    viridis_colors <- viridis_pal(option = "B")(length(unique(nc_cancer_data_petal()$category)))
    
    plot_ly() %>%
      add_trace(
        data = nc_cancer_data_petal(),
        r = ~n,
        theta = ~age_class,
        type = "barpolar",
        color = ~category,
        colors = viridis_colors,
        hovertemplate = paste('n: %{r}', '<br>age_class: %{theta}<br>')
      ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          font = list(size = 10)
        ),
        polar = list(
          bgcolor = "#1f1f1f",
          angularaxis = list(
            rotation = 90,
            direction = 'clockwise',
            categoryorder = "array",
            categoryarray = c("moins de 20 ans", "20-29 ans", "30-39 ans", "40-49 ans", "50-59 ans", "60-69 ans", "70-79 ans", "80 ans et +"),
            period = 8,
            tickcolor = "#ffffff",
            gridcolor = "#444444"
          ),
          radialaxis = list(
            tickcolor = "#ffffff",
            gridcolor = "#444444"
          )
        ),
        paper_bgcolor = "#1f1f1f",
        plot_bgcolor = "#1f1f1f",
        font = list(color = "#ffffff")
      )
  })
  
  nc_cancer_data_grouped_category <- reactive({
    filtered_data() %>%
      group_by(diagnosis_year, category, sex) %>%
      summarise(n = n(), .groups = 'drop')
  })
  
  output$ggplotPlot <- renderPlot({
    ggplot(nc_cancer_data_grouped_category(), aes(x = diagnosis_year, y = n, fill = category)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ sex) +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = "Cancer Cases by Category Over Time", x = "Year", y = "Count", fill = "Category")
  })
}

shinyApp(ui = ui, server = server)
