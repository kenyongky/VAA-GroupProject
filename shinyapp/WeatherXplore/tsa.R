#2 am Working Code
# Load packages
pacman::p_load(shiny, tidyverse, lubridate, tsibble, fable, feasts, fable.prophet, plotly, DT, zoo)

# Load and impute data
weather_data <- read_csv("data/weather_data_cleaned.csv")

weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date) %>%
  group_by_key() %>%
  fill_gaps(.full = TRUE) %>%
  mutate(
    `Daily Rainfall Total (mm)` = zoo::na.approx(`Daily Rainfall Total (mm)`, x = Date, na.rm = FALSE),
    `Mean Temperature (°C)` = zoo::na.approx(`Mean Temperature (°C)`, x = Date, na.rm = FALSE),
    `Maximum Temperature (°C)` = zoo::na.approx(`Maximum Temperature (°C)`, x = Date, na.rm = FALSE),
    `Minimum Temperature (°C)` = zoo::na.approx(`Minimum Temperature (°C)`, x = Date, na.rm = FALSE)
  ) %>% ungroup()

variables_select <- c(
  "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
)

# UI
tsaUI <- function(id) {
  ns <- NS(id)
  
  navbarPage("Time Series Analysis",
             
             # Tab 1: Time Series Visualization
             tabPanel("Time Series Visualization",
                      fluidPage(
                        fluidRow(
                          column(2,
                                 wellPanel(
                                   selectInput(ns("variable"), "Weather Variable:", choices = NULL),
                                   radioButtons(ns("resolution"), "Time Resolution:", choices = c("Daily", "Weekly", "Monthly")),
                                   selectInput(ns("station"), "Select Station(s):", choices = NULL, multiple = TRUE),
                                   dateRangeInput(ns("daterange"), "Date Range:", start = NULL, end = NULL)
                                 )
                          ),
                          column(10,
                                 plotlyOutput(ns("ts_plot"), height = "500px")
                          )
                        )
                      )
             ),
             
             # Tab 2: Updated Decomposition
             tabPanel("Decomposition",
                      fluidPage(
                        fluidRow(
                          column(2,
                                 wellPanel(
                                   selectInput(ns("decomp_var"), "Weather Variable:", choices = NULL),
                                   selectInput(ns("decomp_station"), "Select Station:", choices = NULL),
                                   sliderInput(ns("acf_lag"), "ACF/PACF Lag:", min = 10, max = 100, value = 30)
                                 )
                          ),
                          column(10,
                                 fluidRow(
                                   column(6, plotOutput(ns("acf_plot"), height = "300px")),
                                   column(6, plotOutput(ns("pacf_plot"), height = "300px"))
                                 ),
                                 fluidRow(
                                   column(12, plotlyOutput(ns("stl_plot"), height = "400px"))
                                 )
                          )
                        )
                      )
             ),
             
             # Tab 3: Forecasting
             navbarMenu("Time Series Forecasting",
                        tabPanel("Forecast & Validation",
                                 fluidPage(
                                   fluidRow(
                                     column(2,
                                            wellPanel(
                                              selectInput(ns("forecast_var"), "Weather Variable:", choices = NULL),
                                              selectInput(ns("forecast_station"), "Select Station:", choices = NULL),
                                              radioButtons(ns("time_resolution"), "Time Resolution:", choices = c("Daily", "Weekly", "Monthly")),
                                              sliderInput(ns("train_ratio"), "Train-Test Split:", min = 0.6, max = 0.9, value = 0.8),
                                              checkboxGroupInput(ns("models"), "Forecasting Models:", choices = NULL),
                                              actionButton(ns("run_forecast"), "Generate Forecast")
                                            )
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(6, uiOutput(ns("forecast_title")), plotlyOutput(ns("forecast_plot"), height = "300px")),
                                              column(6, uiOutput(ns("residual_title")), plotOutput(ns("residual_plot"), height = "300px"))
                                            ),
                                            fluidRow(
                                              column(6, uiOutput(ns("test_title")), plotOutput(ns("test_plot"), height = "300px")),
                                              column(6, uiOutput(ns("accuracy_title")), DTOutput(ns("accuracy_tbl"), height = "300px"))
                                            )
                                     )
                                   )
                                 )
                        ),
                        
                        tabPanel("Future Forecast (Refitted)",
                                 fluidPage(
                                   fluidRow(
                                     column(2,
                                            wellPanel(
                                              selectInput(ns("full_var"), "Weather Variable:", choices = NULL),
                                              selectInput(ns("full_station"), "Select Station:", choices = NULL),
                                              radioButtons(ns("horizon_unit"), "Forecast Period Unit:", choices = c("Days" = "day", "Weeks" = "week")),
                                              sliderInput(ns("full_horizon"), "Select Forecast Period:", min = 1, max = 91, value = 30),
                                              checkboxGroupInput(ns("full_models"), "Forecasting Models:", choices = NULL),
                                              actionButton(ns("run_future"), "Forecast")
                                            )
                                     ),
                                     column(10,
                                            uiOutput(ns("future_title")),
                                            plotlyOutput(ns("future_forecast_plot"), height = "300px"),
                                            DTOutput(ns("future_table"), height = "300px")
                                     )
                                   )
                                 )
                        )
             )
  )
}

# Server
tsaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data
    weather_data <- read_csv("data/weather_data_cleaned.csv")
    
    weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date) %>%
      group_by_key() %>%
      fill_gaps(.full = TRUE) %>%
      mutate(
        `Daily Rainfall Total (mm)` = zoo::na.approx(`Daily Rainfall Total (mm)`, x = Date, na.rm = FALSE),
        `Mean Temperature (°C)` = zoo::na.approx(`Mean Temperature (°C)`, x = Date, na.rm = FALSE),
        `Maximum Temperature (°C)` = zoo::na.approx(`Maximum Temperature (°C)`, x = Date, na.rm = FALSE),
        `Minimum Temperature (°C)` = zoo::na.approx(`Minimum Temperature (°C)`, x = Date, na.rm = FALSE)
      ) %>% ungroup()
    
    variables_select <- c(
      "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)",
      "Mean Temperature (°C)" = "Mean Temperature (°C)",
      "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
      "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
    )
    
    models_select <- c(
      "STL + Naive" = "STLNaive",
      "STL + ARIMA" = "STLArima",
      "STL + ETS" = "STLETS",
      "Auto ARIMA" = "AUTOARIMA",
      "Auto Prophet" = "AUTOprophet",
      "Auto ETS" = "AUTOETS"
    )
    
    # Initialize dynamic inputs
    updateSelectInput(session, "variable", choices = variables_select, selected = "Daily Rainfall Total (mm)")
    updateSelectInput(session, "decomp_var", choices = variables_select)
    updateSelectInput(session, "forecast_var", choices = variables_select)
    updateSelectInput(session, "full_var", choices = variables_select)
    
    stations <- unique(weather_tsbl$Station)
    updateSelectInput(session, "station", choices = c("All Stations", stations), selected = "All Stations")
    updateSelectInput(session, "decomp_station", choices = stations)
    updateSelectInput(session, "forecast_station", choices = stations)
    updateSelectInput(session, "full_station", choices = stations)
    
    updateDateRangeInput(session, "daterange",
                         start = min(weather_tsbl$Date),
                         end = max(weather_tsbl$Date))
    
    updateCheckboxGroupInput(session, "models", choices = models_select, selected = c("AUTOARIMA", "AUTOETS"))
    updateCheckboxGroupInput(session, "full_models", choices = models_select, selected = c("AUTOARIMA", "AUTOETS"))
    
    # Tab 1: Visualization
    selected_data <- reactive({
      data <- weather_tsbl
      if (!("All Stations" %in% input$station)) {
        data <- data %>% filter(Station %in% input$station)
      }
      data <- data %>% filter(Date >= input$daterange[1], Date <= input$daterange[2])
      
      if (input$resolution == "Weekly") {
        data %>%
          mutate(Period = floor_date(Date, "week")) %>%
          group_by(Station, Period) %>%
          summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
      } else if (input$resolution == "Monthly") {
        data %>%
          mutate(Period = floor_date(Date, "month")) %>%
          group_by(Station, Period) %>%
          summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
      } else {
        data %>% mutate(Period = Date, Value = .data[[input$variable]]) %>%
          select(Station, Period, Value)
      }
    })
    
    output$ts_plot <- renderPlotly({
      p <- ggplot(selected_data(), aes(x = Period, y = Value, color = Station)) +
        geom_line() + theme_minimal() +
        labs(title = paste("Time Series of", input$variable), x = "Date", y = input$variable)
      ggplotly(p)
    })
    
    # Tab 2: Decomposition
    decomp_data <- reactive({
      weather_tsbl %>%
        filter(Station == input$decomp_station) %>%
        select(Date, value = all_of(input$decomp_var)) %>%
        as_tsibble(index = Date)
    })
    
    output$acf_plot <- renderPlot({
      decomp_data() %>%
        ACF(value, lag_max = input$acf_lag) %>%
        autoplot() +
        ggtitle("ACF Plot") +
        theme_minimal()
    })
    
    output$pacf_plot <- renderPlot({
      decomp_data() %>%
        PACF(value, lag_max = input$acf_lag) %>%
        autoplot() +
        ggtitle("PACF Plot") +
        theme_minimal()
    })
    
    output$stl_plot <- renderPlotly({
      stl_decomp <- decomp_data() %>%
        model(STL(value)) %>%
        components()
      ggplotly(autoplot(stl_decomp) + theme_minimal())
    })
    
    # Tab 3: Forecast & Validation
    observeEvent(input$run_forecast, {
      req(input$models)
      station_data <- weather_tsbl %>%
        filter(Station == input$forecast_station) %>%
        select(Date, value = all_of(input$forecast_var))
      
      train_n <- floor(nrow(station_data) * input$train_ratio)
      train_ts <- as_tsibble(station_data[1:train_n, ], index = Date)
      test_ts <- as_tsibble(station_data[(train_n + 1):nrow(station_data), ], index = Date)
      full_ts <- as_tsibble(station_data, index = Date)
      
      models <- train_ts %>%
        model(
          STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
          STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
          STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
          AUTOARIMA = ARIMA(value),
          AUTOprophet = prophet(value),
          AUTOETS = ETS(value)
        ) %>% select(all_of(input$models))
      
      fc <- forecast(models, h = nrow(test_ts))
      
      output$forecast_plot <- renderPlotly({
        ggplotly(
          autoplot(full_ts, value) +
            autolayer(fc, level = NULL) +
            labs(title = paste("Forecast Validation for", input$forecast_var, "of", input$forecast_station)) +
            theme_minimal()
        )
      })
      
      output$residual_plot <- renderPlot({
        augment(models) %>%
          filter(!is.na(.resid)) %>%
          ggplot(aes(x = .resid)) +
          geom_histogram(bins = 30, fill = "skyblue", color = "black") +
          facet_wrap(~.model, scales = "free") +
          theme_minimal()
      })
      
      output$test_plot <- renderPlot({
        autoplot(test_ts, value) +
          autolayer(fc, level = NULL) +
          labs(title = "Test Forecast vs Actual", x = "Date", y = input$forecast_var) +
          theme_minimal()
      })
      
      output$accuracy_tbl <- renderDT({
        accuracy(fc, test_ts) %>% select(.model, RMSE, MAE, MAPE)
      })
      
      output$residual_title <- renderUI(h4("Residual Plot"))
      output$test_title <- renderUI(h4("Test Forecast vs Actual"))
      output$accuracy_title <- renderUI(h4("Forecast Accuracy"))
    })
    
    # Tab 4: Future Forecast
    observeEvent(input$run_future, {
      req(input$full_models)
      period_unit <- input$horizon_unit
      
      raw_data <- weather_tsbl %>% filter(Station == input$full_station)
      
      if (input$full_var == "Mean Temperature (°C)" && period_unit %in% c("week", "month")) {
        full_ts <- raw_data %>%
          mutate(period = floor_date(Date, unit = period_unit)) %>%
          index_by(period) %>%
          summarise(value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop") %>%
          rename(Date = period)
      } else if (input$full_var == "Daily Rainfall Total (mm)" && period_unit %in% c("week", "month")) {
        full_ts <- raw_data %>%
          mutate(period = floor_date(Date, unit = period_unit)) %>%
          index_by(period) %>%
          summarise(value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
          rename(Date = period)
      } else {
        full_ts <- raw_data %>% select(Date, value = all_of(input$full_var))
      }
      
      full_ts <- as_tsibble(full_ts, index = Date)
      
      models <- full_ts %>%
        model(
          STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
          STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
          STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
          AUTOARIMA = ARIMA(value),
          AUTOprophet = prophet(value),
          AUTOETS = ETS(value)
        ) %>% select(all_of(input$full_models))
      
      fc <- forecast(models, h = paste0(input$full_horizon, " ", period_unit))
      
      output$future_forecast_plot <- renderPlotly({
        ggplotly(
          autoplot(fc, level = 95) +
            labs(title = paste("Future Forecast for", input$full_var, "of", input$full_station)) +
            theme_minimal(),
          tooltip = c("x", "y", ".model")
        )
      })
      
      output$future_table <- renderDT({
        as.data.frame(fc)
      })
      
      output$future_title <- renderUI(h4("Future Forecast Plot"))
    })
  })
}







