# Libraries
library(leaflet)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Discover Singapore"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "App Guide", tabName = "guide", icon = icon("info")),
      menuItem(text = "Map View", tabName = "mapview", icon = icon("map")),
      menuItem(text = "Tripadvisor Reviews", tabName = "tripadvisor", icon = icon("binoculars"))
    )
  ),
  
  dashboardBody(
    tabItem(tabName = "guide"
      # Application Guide
      
    ),
    
    tabItems(
      # Map view tab
      tabItem(tabName = "mapview",
        sidebarLayout(
          position = "right",
          sidebarPanel(
            checkboxGroupInput(
              inputId = "transportOptions",
              label = "Transportation:",
              choiceNames = c("Bus Stops", "MRT/LRT Stations", "Taxi Stands"),
              choiceValues = c(1, 2, 3)
            ),
            checkboxGroupInput(
              inputId = "medicalOptions",
              label = "Medical:",
              choiceNames = c("Clinics", "Hospitals"),
              choiceValues = c(4, 5)
            ),
            checkboxGroupInput(
              inputId = "touristOptions",
              label = "Tourism:",
              choiceNames = c("Hawker Centres", "Historic Sites", "Hotels", "Travel Attractions"),
              choiceValues = c(6, 7, 8, 9)
            )
          ),
        
          mainPanel(
            leafletOutput(outputId = "map"),
          )
        ),
        
        fluidRow(
          column(8, offset = 0, style = "padding: 20px;", uiOutput(outputId = "information")),
          column(4, )
        )
      ),
      
      # Twitter tab
      tabItem(tabName = "twitter"
      )
    )
  )
)

server <- function(input, output){
  # Load data
  attractions <- read.csv("./datasets/tourist attractions/TOURISM.csv", stringsAsFactors = F)
  busstops <- read.csv("./datasets/bus stops/BusStop.csv", stringsAsFactors = F)
  clinics <- read.csv("./datasets/chas clinics/chas-clinics.csv", stringsAsFactors = F)
  hawker_centres <- read.csv("./datasets/hawker centres/hawker-centres.csv", stringsAsFactors = F)
  historic_sites <- read.csv("./datasets/historic sites/historic-sites.csv", stringsAsFactors = F)
  hospitals <- read.csv("./datasets/hospitals/hospital_data.csv", stringsAsFactors = F)
  hotels <- read.csv("./datasets/hotels/hotels.csv", stringsAsFactors = F)
  stations <- read.csv("./datasets/train stations/MRTLRTstations.csv", stringsAsFactors = F)
  taxi_stands <- read.csv("./datasets/taxi stands/TaxiStop.csv", stringsAsFactors = F)
  combined_data <- read.csv("./datasets/combined_data.csv", stringsAsFactors = F)
  
  # Filters combined data frame by checkbox selection
  filtered_data <- reactive({
    viewOptions <- c(input$transportOptions, input$medicalOptions, input$touristOptions)
    combined_data %>% filter(., Group %in% viewOptions)
  })
  
  # List of icons for leaflet map
  map_icons <- awesomeIconList(
    busstop = makeAwesomeIcon(icon = "bus", library = "fa", 
                              markerColor = "gray", iconColor = "#FFFFFF"),
    station = makeAwesomeIcon(icon = "subway", library = "fa", 
                              markerColor = "black", iconColor = "#FFFFFF"),
    taxi = makeAwesomeIcon(icon = "taxi", library = "fa", 
                           markerColor = "lightgray", iconColor = "#FFFFFF"),
    clinic = makeAwesomeIcon(icon = "plus-square", library = "fa", 
                             markerColor = "orange", iconColor = "#FFFFFF"),
    hospital = makeAwesomeIcon(icon = "h-square", library = "fa",
                               markerColor = "red", iconColor = "#FFFFFF"),
    hawker = makeAwesomeIcon(icon = "cutlery", library = "glyphicon", 
                             markerColor = "purple", iconColor = "#FFFFFF"),
    historic = makeAwesomeIcon(icon = "bank", library = "fa",
                               markerColor = "darkblue", iconColor = "#FFFFFF"),
    hotels = makeAwesomeIcon(icon = "hotel", library = "fa", 
                             markerColor = "darkgreen", iconColor = "#FFFFFF"),
    tourist = makeAwesomeIcon(icon = "camera", library = "fa", 
                              markerColor = "blue", iconColor = "#FFFFFF")
  )
  
  # Plot markers of locations on the leaflet map
  plotLocations <- function(m) {
    m %>% addAwesomeMarkers(data = filtered_data(), lng = ~Longitude, lat = ~Latitude, 
                            layerId = ~paste0(Group, "-", Row_ID), icon = ~map_icons[Group], 
                            popup = ~Name, clusterOptions = markerClusterOptions())
  }
  
  output$map <- renderLeaflet({
    # Set map view to Singapore's coordinates
    leaflet() %>% addTiles() %>% setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  # Retains zoom view of map when user selects additional options
  observe({
    leafletProxy("map") %>% clearMarkerClusters() %>% plotLocations()
  })
  
  # Observes the leaflet marker that the user clicks on
  observeEvent(input$map_marker_click, {
    ref <- unlist(strsplit(input$map_marker_click[[1]], split = "-"))
    data_index <- as.numeric(ref[1])
    row_index <- as.numeric(ref[2])
    
    # List of data frames
    data <- lapply(list(busstops, stations, taxi_stands, clinics, hospitals, hawker_centres, 
                        historic_sites, hotels, attractions), (\(x) sapply(x, as.character)))

    # Retrieve row in selected data frame
    selected_row <- data[[data_index]][row_index, ]
    
    # Displays information regarding the selected marker
    output$information <- renderUI({
      switch(data_index,
        busstop = {
          # Case when a Bus Stop is selected
          p(strong("Bus Stop Code: "), selected_row[2], br(),
            strong("Bus Roof Code: "), selected_row[3], br(),
            strong("Street Name: "), selected_row[4], br(),
            strong("Longitude: "), selected_row[5], br(),
            strong("Latitude: "), selected_row[6]
            )
        }, 
        station = {
          # Case when a station is selected
          p(strong("Station Name: "), selected_row[2], br(),
            strong("Station Number: "), selected_row[3], br(),
            strong("Longitude: "), selected_row[4], br(),
            strong("Latitude: "), selected_row[5]
            )
        },
        taxi = {
          # Case when a taxi stand is selected
          p(strong("Type: "), selected_row[2], br(),
            strong("Longitude: "), selected_row[3], br(),
            strong("Latitude: "), selected_row[4]
          )
        },
        clinic = {
          # Case when a clinic is selected
          p(strong("HCI Code: "), selected_row[2], br(),
            strong("Clinic Name: "), selected_row[3], br(),
            strong("License Type: "), selected_row[4], br(),
            strong("Contact: "), selected_row[5], br(),
            strong("Program Code: "), selected_row[13], br(),
            strong("Address: "), selected_row[20], br(),
            strong("Longitude: "), selected_row[18], br(),
            strong("Latitude: "), selected_row[19]
          )
        },
        hospital = {
          # Case when a hospital is selected
          p(strong("Name: "), selected_row[2], br(),
            strong("Address: "), selected_row[3], br(),
            strong("Contact: "), selected_row[4], br(),
            strong("Longitude: "), selected_row[6], br(),
            strong("Latitude: "), selected_row[5]
          )
        },
        hawker = {
          # Case when a hawker centre is selected
          p(strong("Name: "), selected_row[13], br(),
            strong("Address: "), selected_row[21], br(),
            strong("Longitude: "), selected_row[7], br(),
            strong("Latitude: "), selected_row[3]
          )
        },
        historic = {
          # Case when a historic site is selected
          p(strong("Name: "), selected_row[6], br(),
            strong("Description: "), selected_row[8], br(),
            strong("Website: "), a("Click here", href = selected_row[7], target="_blank"), br(),
            strong("Longitude: "), selected_row[14], br(),
            strong("Latitude: "), selected_row[15],
          )
        },
        hotel = {
          # Case when a hotel is selected
          p(strong("Name: "), selected_row[2], br(),
            strong("Total rooms: "), selected_row[9], br(),
            strong("Email: "), selected_row[7], br(),
            strong("Keeper Name: "), selected_row[8], br(),
            strong("Longitude: "), selected_row[3], br(),
            strong("Latitude: "), selected_row[4],
          )
        },
        attraction = {
          # Case when a tourist attraction is selected
          p(strong("Name: "), selected_row[2], br(),
            strong("Street: "), selected_row[6], br(),
            strong("Opening Hours: "), selected_row[5], br(),
            strong("Description: "), selected_row[7], br(),
            strong("Website: "), a("Click here", href = selected_row[9], target="_blank"), br(),
            strong("Longitude: "), selected_row[4], br(),
            strong("Latitude: "), selected_row[3],
          )
        },
        {
          
        }
      )
    })
  })
}

shinyApp(ui=ui, server=server)