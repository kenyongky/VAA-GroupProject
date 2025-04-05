# Full EDA + CDA Shiny App with Improved Violin Overlay and Interactive Plot

# Load necessary packages
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggstatsplot)
library(parameters)
library(nortest)

# Load and preprocess data
weather <- read_csv("data/weather_data_cleaned.csv") %>%
  rename(
    MaxTemp = `Maximum Temperature (°C)`,
    MinTemp = `Minimum Temperature (°C)`,
    MeanTemp = `Mean Temperature (°C)`,
    Rainfall = `Daily Rainfall Total (mm)`
  ) %>%
  mutate(
    Date = ymd(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    MonthNum = month(Date),
    TempRange = MaxTemp - MinTemp
  )

# UI
edaUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("WeatherXplore - EDA & CDA"),
    tabsetPanel(
      tabPanel("Exploratory Analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(ns("eda_scenario"), "Select EDA Scenario:",
                               choices = c("Monthly Average Temperature & Rainfall by Year",
                                           "Temperature Range by Station",
                                           "Top 10 Rainiest Days Each Year")),
                   
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'Monthly Average Temperature & Rainfall by Year'", ns("eda_scenario")),
                     sliderInput(ns("year_range"), "Year Range:", 2020, 2024, c(2020, 2024), step = 1, sep = ""),
                     sliderInput(ns("month_range"), "Month Range:", 1, 12, c(1, 12), step = 1, sep = ""),
                     uiOutput(ns("station_eda_ui")),
                     actionButton(ns("plot_eda"), "Plot Graph")
                   ),
                   
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'Temperature Range by Station'", ns("eda_scenario")),
                     uiOutput(ns("region_input_ui")),
                     uiOutput(ns("station_temp_range_ui")),
                     sliderInput(ns("year_temp_range"), "Year Range:", 2020, 2024, c(2020, 2024), sep = ""),
                     sliderInput(ns("month_temp_range"), "Month Range:", 1, 12, c(1, 12), sep = ""),
                     actionButton(ns("plot_temp_range"), "Plot Graph")
                   ),
                   
                   conditionalPanel(
                     condition = sprintf("input['%s'] == 'Top 10 Rainiest Days Each Year'", ns("eda_scenario")),
                     selectInput(ns("year_rainiest"), "Year:", 2020:2024, selected = 2020),
                     actionButton(ns("plot_rainiest"), "Plot Graph")
                   )
                 ),
                 mainPanel(plotlyOutput(ns("eda_plot")))
               )
      ),
      
      tabPanel("Confirmatory Analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(ns("cda_variable"), "Choose Variable:",
                               choices = c("Mean Temperature (°C)" = "MeanTemp",
                                           "Maximum Temperature (°C)" = "MaxTemp",
                                           "Minimum Temperature (°C)" = "MinTemp",
                                           "Daily Rainfall Total (mm)" = "Rainfall")),
                   uiOutput(ns("cda_stations_ui")),
                   uiOutput(ns("cda_years_ui")),
                   selectInput(ns("stat_approach"), "Statistical Approach:",
                               choices = c("parametric", "nonparametric", "robust", "bayes")),
                   selectInput(ns("conf_level"), "Confidence Level:",
                               choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95),
                   textInput(ns("plot_title"), "Plot Title", "Station-wise Comparison"),
                   actionButton(ns("run_test"), "Run Test")
                 ),
                 mainPanel(
                   plotlyOutput(ns("cda_plot")),
                   verbatimTextOutput(ns("test_output"))
                 )
               )
      )
    )
  )
}


# Server
edaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    weather <- read_csv("data/weather_data_cleaned.csv") %>%
      rename(MaxTemp = `Maximum Temperature (°C)`,
             MinTemp = `Minimum Temperature (°C)`,
             MeanTemp = `Mean Temperature (°C)`,
             Rainfall = `Daily Rainfall Total (mm)`) %>%
      mutate(Date = ymd(Date),
             Year = year(Date),
             Month = month(Date, label = TRUE, abbr = TRUE),
             MonthNum = month(Date),
             TempRange = MaxTemp - MinTemp)
    
    output$station_eda_ui <- renderUI({
      selectInput(ns("station_eda"), "Stations:",
                  choices = unique(weather$Station),
                  selected = unique(weather$Station)[1], multiple = TRUE)
    })
    
    output$region_input_ui <- renderUI({
      selectInput(ns("region_input"), "Region:",
                  choices = unique(weather$Region), selected = unique(weather$Region), multiple = TRUE)
    })
    
    output$station_temp_range_ui <- renderUI({
      selectInput(ns("station_temp_range"), "Stations:",
                  choices = unique(weather$Station), selected = unique(weather$Station), multiple = TRUE)
    })
    
    output$cda_stations_ui <- renderUI({
      selectInput(ns("cda_stations"), "Stations:",
                  choices = unique(weather$Station), selected = unique(weather$Station)[1:2], multiple = TRUE)
    })
    
    output$cda_years_ui <- renderUI({
      selectInput(ns("cda_years"), "Years:",
                  choices = unique(weather$Year), selected = unique(weather$Year)[1:2], multiple = TRUE)
    })
    
    observeEvent(input$plot_eda, {
      output$eda_plot <- renderPlotly({
        if (input$eda_scenario == "Monthly Average Temperature & Rainfall by Year") {
          filtered <- weather %>%
            filter(Year >= input$year_range[1], Year <= input$year_range[2],
                   MonthNum >= input$month_range[1], MonthNum <= input$month_range[2],
                   Station %in% input$station_eda) %>%
            group_by(Year, Month) %>%
            summarise(AvgTemp = mean(MeanTemp, na.rm = TRUE),
                      TotalRain = sum(Rainfall, na.rm = TRUE), .groups = 'drop') %>%
            pivot_longer(cols = c(AvgTemp, TotalRain), names_to = "Variable", values_to = "Value")
          
          p <- ggplot(filtered, aes(x = Month, y = Value, color = factor(Year), group = Year)) +
            geom_line(linewidth = 1) +
            facet_wrap(~Variable, scales = "free_y") +
            theme_minimal() +
            labs(title = "Monthly Average Temperature and Total Rain", x = "Month", y = NULL, color = "Year")
          ggplotly(p)
          
        } else if (input$eda_scenario == "Temperature Range by Station") {
          filtered <- weather %>%
            filter(Region %in% input$region_input,
                   Station %in% input$station_temp_range,
                   Year >= input$year_temp_range[1], Year <= input$year_temp_range[2],
                   MonthNum >= input$month_temp_range[1], MonthNum <= input$month_temp_range[2])
          
          p <- ggplot(filtered, aes(x = Date, y = TempRange, color = Region)) +
            geom_line(alpha = 0.6) + theme_minimal()
          ggplotly(p)
          
        } else if (input$eda_scenario == "Top 10 Rainiest Days Each Year") {
          filtered <- weather %>%
            filter(Year == input$year_rainiest) %>%
            slice_max(Rainfall, n = 10, with_ties = FALSE) %>%
            arrange(desc(Rainfall))
          
          p <- ggplot(filtered, aes(x = reorder(Station, Rainfall), y = Rainfall, fill = Station)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal() +
            labs(title = paste("Top 10 Rainiest Days in", input$year_rainiest), y = "Rainfall (mm)", x = "Station")
          ggplotly(p)
        }
      })
    })
    
    observeEvent(input$run_test, {
      req(input$cda_variable, input$cda_stations, input$cda_years)
      
      var <- input$cda_variable
      conf_level <- as.numeric(input$conf_level)
      
      data_filtered <- weather %>%
        filter(Station %in% input$cda_stations, Year %in% input$cda_years) %>%
        select(Station, Year, value = .data[[var]])
      
      output$cda_plot <- renderPlotly({
        p <- ggplot(data_filtered, aes(x = Station, y = value, fill = Station)) +
          geom_violin(alpha = 0.5, color = NA) +
          geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.6) +
          geom_jitter(width = 0.2, height = 0, alpha = 0.4, size = 1, color = "black") +
          labs(title = input$plot_title, x = "Station", y = var) +
          theme_minimal() +
          theme(legend.position = "none")
        ggplotly(p, tooltip = c("x", "y"))
      })
      
      output$test_output <- renderPrint({
        if (input$stat_approach == "parametric") {
          if (length(unique(data_filtered$Station)) == 2) {
            print(t.test(value ~ Station, data = data_filtered))
          } else {
            print(summary(aov(value ~ Station, data = data_filtered)))
          }
        } else if (input$stat_approach == "nonparametric") {
          if (length(unique(data_filtered$Station)) == 2) {
            print(wilcox.test(value ~ Station, data = data_filtered))
          } else {
            print(kruskal.test(value ~ Station, data = data_filtered))
          }
        } else if (input$stat_approach == "robust") {
          print(oneway_test(value ~ Station, data = data_filtered))
        } else if (input$stat_approach == "bayes") {
          print(bayesfactor_parameters(t_test(value ~ Station, data = data_filtered)))
        }
      })
    })
  })
}
