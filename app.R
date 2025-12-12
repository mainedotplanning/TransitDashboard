# Load required libraries
library(shiny)
library(bslib)
library(ggplot2)
library(tmap)
library(dplyr)
library(tidyverse)
library(paletteer)
library(scales)
library(sf)
library(thematic)
library(leaflet)

# Prepare dataset

transit_agencies_abbrv = c("BANGOR","BSOOB","DTI","LATC","METRO","SPBS","Waldo", "WMTS")

data <- read.csv("data/Transit Metrics by Agency 2014-2023.csv")
data <- data %>% filter(Provider %in% transit_agencies_abbrv)
data_long <- pivot_longer(data[,1:7], cols = 3:7, names_to = "Metric", values_to= "Values")


# Create a color mapping for agencies
mappal <- colorFactor(palette = c("#FFA500",
                                   "#FF69B4",
                                   "#00CED1",
                                   "#1dacd6",
                                   "#483D8B",
                                   "#98FB98",
                                   "#800080",
                                   "#006400",
                                    "red"
                                    ),
                       levels = c("Biddeford Saco Old Orchard Beach Transit Committee Shuttle Bus",
                                  "City of Bangor",
                                  "Downeast Transportation Inc",
                                  "Lewiston Auburn Transit Committee",
                                  "Greater Portland Transit District",
                                  "City of South Portland",
                                  "Waldo Community Action Partners",
                                  "Western Maine Transportation Services Inc",
                                  "Wests Transportation Inc"
                       )
)



# Create a color mapping for agencies
plotpal <- c("BSOOB"= "#FFA500",
             "BANGOR"= "#FF69B4",
             "DTI"= "#00CED1",
             "LATC"= "#1dacd6" ,
             "METRO"= "#483D8B",
             "WMTS"= "#006400",
             "SPBS"= "#98FB98",
             "Waldo"= "#800080")
  

variable = list("Annual_Unlinked_Trips" = "Annual_Unlinked_Trips", 
     "Annual_Vehicle_Revenue_Miles" = "Annual_Vehicle_Revenue_Miles", 
     "Annual_Vehicle_Revenue_Hours" = "Annual_Vehicle_Revenue_Hours", 
     "Operating_Expenses" = "Operating_Expenses", 
     "Fare_Revenue" = "Fare_Revenue")

# Map of Transit Providers

# Transit Routes and Stops

load("data/maine_transit_stopsandroutes.rData" )

# Define UI
ui <- page_navbar(
  
  title = "Maine Transit Data Dashboard, 2014-2023 NTD data",

  nav_panel(
    "Operational Performance",
    layout_sidebar(
      
      sidebar = sidebar(
        title = "Graph controls",
        width = 350,
    
      # Variable selector
      selectInput("prov", "Select Provider:", 
                  choices = transit_agencies_abbrv,
                  selected = c("BANGOR", "SPBS"),
                  multiple = TRUE),
      
      # Variable selector
      selectInput("varA", "Select Variable A:", choices = variable),
      selectInput("varB", "Select Variable B:", choices = variable),
      
      # Time range slider
      sliderInput(
        "time_range", "Select Date Range:",
        min = 2014,
        max = 2023,
        value = c(2014, 2023),
        timeFormat = "%Y",  # Display format
        step = 1
      )
    ),
    
    fluidRow( 
      column(width = 6, 
             card(height = 250,
                  card_header(textOutput("dynamic_headerA")),
                  plotOutput("plotA")
             )
      ),
      column(width = 6, 
             card(height = 250,
                  card_header(textOutput("dynamic_headerB")),
                  plotOutput("plotB")
             )
      )
    ),
    fluidRow(
      column(width = 12, 
             card(height = 450,
                  card_header(textOutput("dynamic_headerC")),
                  plotOutput("ratioPlot")
             )))
    )),
  
  nav_panel(
    "Map of Service Areas",
    layout_sidebar(
      
      sidebar = sidebar(
        title = "Map controls",
        width = 350,
        
        selectInput(
          "agency", "Select Agency",
          choices = unique(GTFS_routes$Agency_Name),
          selected = "", multiple = F
        ),
        
        checkboxGroupInput("routes", "Select Routes:", choices = NULL),
        
        br(),
        
        
        
      ),
      
      fluidRow(
        column(width = 12, 
               card(height = 450,
                    card_header("Map of Routes and Stops"),
                    leafletOutput("serviceMap")
               )))
    ))
)

# Define Server
server <- function(input, output, session) {
  
# -------------Operational Performance Data  ------------
  
  # Reactive filtered data
  
  filtered_data_a <- reactive({
    req(input$prov, input$varA, input$time_range)
    data_long %>%
      filter(Metric==input$varA, Provider %in% input$prov, Year >= input$time_range[1], Year <= input$time_range[2])
  })
  
  filtered_data_b <- reactive({
    req(input$prov, input$varB, input$time_range)
    data_long %>%
      filter(Metric==input$varB, Provider %in% input$prov, Year >= input$time_range[1], Year <= input$time_range[2])
  
  })
  
  filtered_data_c <- reactive({
    req(input$prov, input$varA, input$varB, input$time_range)
    
            # Prevent same column selection
            validate(
              need(input$varA != input$varB, "Please select two different columns.")
            )
            
            # Ensure numeric columns
            validate(
              need(is.numeric(data[[input$varA]]) && is.numeric(data[[input$varB]]),
                   "Both selected columns must be numeric.")
            )
            
            # Avoid division by zero
            validate(
              need(all(data[[input$varB]] != 0), "Denominator column contains zero values.")
            )
    
    data[, c('Provider','Year', input$varA, input$varB), drop = FALSE]

    
    data %>%
      filter(Provider %in% input$prov, Year >= input$time_range[1], Year <= input$time_range[2]) %>%
      mutate(ratio = .data[[input$varA]] / .data[[input$varB]])
    
    
  })
  
  # Render plot
  output$plotA <- renderPlot({
    dfa <- filtered_data_a()
    
    ggplot(dfa, aes(x = Year, y = Values, color = Provider, group = Provider)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) + 
      scale_colour_manual(values = plotpal)+
      scale_x_continuous(limits = c(2014,2023), breaks = seq(2014,2023,1), minor_breaks = NULL)+
      scale_y_continuous(limits = c(0,max(dfa$Values)), 
                                   labels = function(x) format(x, scientific = FALSE, big.mark = ","))+
      labs(
        title = paste("Time Series of", input$varA),
        x = "Year",
        y = input$varA
      ) +
      theme_minimal()
      
  })
  output$plotB <- renderPlot({
    dfb <- filtered_data_b()
    
    ggplot(dfb, aes(x = Year, y = Values, color = Provider, group = Provider)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) + 
      scale_colour_manual(values = plotpal) +
      scale_x_continuous(limits = c(2014,2023), breaks = seq(2014,2023,1), minor_breaks = NULL)+
      scale_y_continuous(limits = c(0,max(dfb$Values)), 
                         labels = function(x) format(x, scientific = FALSE, big.mark = ","))+
      labs(
        title = paste("Time Series of", input$varB),
        x = "Year",
        y = input$varB
      ) +
      theme_minimal()
      
  })
  
  output$ratioPlot <- renderPlot({
  dfc <- filtered_data_c()
    
    ggplot(dfc, aes(x = Year, y = ratio, color = Provider, group = Provider)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_colour_manual(values = plotpal) +
      scale_x_continuous(limits = c(2014,2023), breaks = seq(2014,2023,1), minor_breaks = NULL)+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))+
      labs(title = paste("Ratio of", input$varA, "to", input$varB),
           y = paste(input$varA, "/", input$varB)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  
  
  
  
  # # ------------------map plots----------------------------
  # 
  # 
  # 
  # output$serviceMap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(providers$CartoDB.Positron, group= "Light Canvas")  %>%
  #     addTiles(group= "OpenStreetMap")  %>%
  #     setView(lng = -68.5,lat=45.3, zoom = 7) %>%
  #     addLayersControl(
  #       baseGroups = c( "Light Canvas", "OpenStreetMap"),
  #       overlayGroups = c("Routes","Stops", "Walksheds" ),
  #       options = layersControlOptions(collapsed = FALSE), position = "topright") 
  # })
  # 
  # 
  # 
  # 
  # # Update checkbox choices based on selected category
  # observeEvent(input$agency, {
  #   
  #   filtered_routes <- unique(GTFS_routes$route_long[GTFS_routes$Agency_Name == input$agency])
  #   updateCheckboxGroupInput(session, "routes", choices = filtered_routes)
  #   
  #   agency_routes <- GTFS_routes[GTFS_routes$Agency_Name == input$agency,]
  #   agency_bbox = st_bbox(agency_routes)
  #   
  #   # Update map markers based on filtered data - change to zoom to agency and select routes in next step. 
  #   leafletProxy("serviceMap") %>%
  #     clearShapes() %>%
  #     setView(lng = agency_bbox$xmin, lat = agency_bbox$ymin, zoom = 13)
  #   
  # })
  # 
  # 
  # # Update map on selected routes
  # observeEvent(input$routes, {
  #   selected_routes <- GTFS_routes$route_long[GTFS_routes$route_long == input$routes]
  #   
  #   mapped_routes <- GTFS_routes[GTFS_routes$route_long == input$routes,]
  #   
  #   # Update map markers based on filtered data - change to zoom to agency and select routes in next step. 
  #   leafletProxy("serviceMap") %>%
  #     addPolylines(data = mapped_routes,
  #                  color = ~mappal(mapped_routes$Agency_Name),
  #                  opacity = 1,
  #                  popup = ~paste0("Agency: ", as.character(mapped_routes$Agency_Name), "<br/>",
  #                                  "Route: ", as.character(mapped_routes$route_long)),
  #                  group = "Routes"
  #     )
  #   
  # })
  # 
  
  
  
  
  # ------------------map plots----------------------------
  
  output$serviceMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Canvas")  %>%
      addTiles(group = "OpenStreetMap")  %>%
      setView(lng = -68.5, lat = 45.3, zoom = 7) %>%
      addLayersControl(
        baseGroups = c("Light Canvas", "OpenStreetMap"),
        overlayGroups = c("Routes", "Stops", "Walksheds"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  })
  
  # Update checkbox choices and zoom to agency bbox when agency changes
  observeEvent(input$agency, {
    req(input$agency)
    
    # build choices for routes for that agency
    filtered_routes <- sort(unique(GTFS_routes$route_long[GTFS_routes$Agency_Name == input$agency]))
    updateCheckboxGroupInput(session, "routes", choices = filtered_routes, selected = NULL)
    
    # subset routes for bbox; make sure there are geometries
    agency_routes <- GTFS_routes[GTFS_routes$Agency_Name == input$agency, ]
    if (nrow(agency_routes) == 0) {
      return()
    }
    
    agency_bbox <- st_bbox(agency_routes)
    
    # Clear previous route/stop layers and zoom to agency using fitBounds
    leafletProxy("serviceMap") %>%
      clearGroup("Routes") %>%
      clearGroup("Stops") %>%
      # fitBounds expects: lng1, lat1, lng2, lat2
      fitBounds(lng1 = agency_bbox$xmin,
                lat1 = agency_bbox$ymin,
                lng2 = agency_bbox$xmax,
                lat2 = agency_bbox$ymax)
  })
  
  # Draw selected routes
  observeEvent(input$routes, {
    req(input$routes)  # nothing to do if no selection
    
    # filter using %in%
    mapped_routes <- GTFS_routes %>% filter(route_long %in% input$routes)
    
    # if nothing matches, clear group and return
    if (nrow(mapped_routes) == 0) {
      leafletProxy("serviceMap") %>% clearGroup("Routes")
      return()
    }
    
    # clear previous routes then add new polylines
    leafletProxy("serviceMap") %>%
      clearGroup("Routes") %>%
      addPolylines(data = mapped_routes,
                   color = ~mappal(Agency_Name),   # use the column inside the data
                   weight = 3,
                   opacity = 1,
                   popup = ~paste0("Agency: ", Agency_Name, "<br/>", "Route: ", route_long),
                   group = "Routes")
  })
  
}

# Run the app
shinyApp(ui, server)
