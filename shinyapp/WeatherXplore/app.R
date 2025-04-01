library(shiny)
library(leaflet)

# Source your module UI/server
source("eda.R")
source("tsa.R")
source("geofacet.R")
source("interpolation.R")

ui <- navbarPage(
  title = div("🌦️ WeatherXplore", style = "display: inline;"),
  
  header = tags$head(
    tags$style(HTML("
      /* Pastel-themed navbar */
      .navbar {
        background-color: #f6f8fa !important;  /* light pastel gray-blue */
        border-bottom: 2px solid #cce5ff;
      }
      .navbar .navbar-brand,
      .navbar-nav > li > a {
        color: #336699 !important;  /* muted pastel blue text */
        font-weight: 500;
      }
      .navbar .navbar-brand:hover,
      .navbar-nav > li > a:hover {
        color: #6699cc !important;  /* lighter blue on hover */
      }
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover,
      .navbar-nav > .active > a:focus,
      .navbar-nav .dropdown-menu > .active > a {
        background-color: #ddeeff !important;
        color: #003366 !important;
      }

      /* Light pastel background */
      body {
        background-color: #fefeff !important;
      }

      
      .container-fluid {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 0 6px rgba(0, 0, 0, 0.04);
      }
    "))
  ),
  
  tabPanel("Home",
           fluidPage(
             h2("Welcome to WeatherXplore!", style = "margin-top: 20px;"),
             p("WeatherXplore is a visual analytics tool that helps you explore, analyze, and interpret historical weather data across Singapore. Use this app to uncover spatial and temporal trends in rainfall and temperature, and gain insights using interactive charts and maps.",
               style = "font-size: 16px; max-width: 900px;"),
             
             br(),
             
             h4("What you can do"),
             tags$ul(
               tags$li(strong("Exploratory Data Analysis:"), " Compare rainfall and temperature across months, years, and stations."),
               tags$li(strong("Confirmatory Analysis:"), " Apply statistical tests to weather variables across locations or time."),
               tags$li(strong("Time Series Analysis:"), " Understand weather patterns over time and make predictions."),
               tags$li(strong("Geospatial Analysis:"), " Visualize spatial distribution using geofacet plots and interpolation maps.")
             ),
             
             br(),
             
             # Compact Left-Aligned Cards
             fluidRow(
               column(3,
                      h4("🌡️ Hottest Day"),
                      div(
                        style = "
        background-color: #fff3cd;
        border-left: 5px solid #ffa500;
        padding: 12px;
        font-size: 14px;
        max-width: 300px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      ",
                        textOutput("record_temp")
                      )
               ),
               column(3,
                      h4("🧊 Coldest Day"),
                      div(
                        style = "
        background-color: #d1ecf1;
        border-left: 5px solid #0c5460;
        padding: 12px;
        font-size: 14px;
        max-width: 300px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      ",
                        textOutput("record_cold")
                      )
               )
             ),
             
             fluidRow(
               column(3,
                      h4("🌧️ Wettest Month"),
                      div(
                        style = "
        background-color: #e2f0d9;
        border-left: 5px solid #3c763d;
        padding: 12px;
        font-size: 14px;
        max-width: 300px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      ",
                        textOutput("record_wet")
                      )
               ),
               column(3,
                      h4("🌤️ Driest Month"),
                      div(
                        style = "
        background-color: #fbeee6;
        border-left: 5px solid #e69138;
        padding: 12px;
        font-size: 14px;
        max-width: 300px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      ",
                        textOutput("record_dry")
                      )
               )
             ),
             
             br(),
             
             br(),
             
             h4("Weather Station Map"),
             p("Explore the map below to see where Singapore's weather stations are located.",
               style = "font-size: 16px; max-width: 900px;"),
             leafletOutput("station_map", width = "900px", height = "500px"),
             
             br()
           )
  ),
  
  tabPanel("Exploratory & Confirmatory Data Analysis", edaUI("eda")),
  tabPanel("Time Series Analysis", tsaUI("tsa")),
  navbarMenu("Geospatial Analysis",
             tabPanel("Geofacet", geofacetUI("geo")),
             tabPanel("Spatial Interpolation", interpolationUI("interp"))
  )
)

server <- function(input, output, session) {
  edaServer("eda")
  tsaServer("tsa")
  geofacetServer("geo")
  interpolationServer("interp")
  
  output$station_map <- renderLeaflet({
    station_coords <- weather_data %>%
      select(Station, Latitude, Longitude) %>%
      distinct()
    
    leaflet(data = station_coords) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 6,
        label = ~Station,
        color = "black",
        fillColor = "orange",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.9
      ) %>%
      setView(lng = 103.8198, lat = 1.35, zoom = 11)
  })
  output$record_temp <- renderText({
    record <- weather_data %>%
      filter(!is.na(`Maximum Temperature (°C)`)) %>%
      arrange(desc(`Maximum Temperature (°C)`), desc(Date)) %>%
      slice(1)  # latest among ties
    
    temp <- record$`Maximum Temperature (°C)`
    station <- record$Station
    date <- format(record$Date, "%d %b %Y")
    
    paste0(temp, " °C at ", station, " on ", date)
  })
  output$record_cold <- renderText({
    record <- weather_data %>%
      filter(!is.na(`Minimum Temperature (°C)`)) %>%
      arrange(`Minimum Temperature (°C)`, desc(Date)) %>%
      slice(1)
    
    paste0(record$`Minimum Temperature (°C)`, " °C at ", record$Station, " on ", format(record$Date, "%d %b %Y"))
  })
  
  # Wettest Month
  output$record_wet <- renderText({
    monthly_rain <- weather_data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(TotalRain = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalRain)) %>%
      slice(1)
    
    paste0(round(monthly_rain$TotalRain, 1), " mm in ", format(monthly_rain$YearMonth, "%b %Y"))
  })
  
  # Driest Month
  output$record_dry <- renderText({
    monthly_rain <- weather_data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(TotalRain = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
      arrange(TotalRain) %>%
      slice(1)
    
    paste0(round(monthly_rain$TotalRain, 1), " mm in ", format(monthly_rain$YearMonth, "%b %Y"))
  })
  
}

shinyApp(ui, server)
