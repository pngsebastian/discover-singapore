# Libraries
library(leaflet)
library(shiny)

ui <- fluidPage(
  titlePanel("Discover Singapore"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "transportOptions",
        label = "Transportation:",
        choices = c("Bus Stops", "MRT/LRT Stations", "Taxi Stands")
      ),
      checkboxGroupInput(
        inputId = "medicalOptions",
        label = "Medical:",
        choices = c("Clinics", "Hospitals")
      ),
      checkboxGroupInput(
        inputId = "touristOptions",
        label = "Tourism:",
        choices = c("Hawker Centres", "Historic Sites", "Hotels", "Travel Attractions")
      )
    ),
  
    mainPanel(
      leafletOutput(outputId = "plotLocations")
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
  taxi_stand <- read.csv("./datasets/taxi stands/TaxiStop.csv", stringsAsFactors = F)
  
  # List of icons for leaflet map
  map_icons <- awesomeIconList(
    busstop = makeAwesomeIcon(icon = "bus", library = "fa", 
                              markerColor = "gray", iconColor = "#FFFFFF"),
    clinic = makeAwesomeIcon(icon = "plus-square", library = "fa", 
                             markerColor = "orange", iconColor = "#FFFFFF"),
    hawker = makeAwesomeIcon(icon = "cutlery", library = "glyphicon", 
                             markerColor = "purple", iconColor = "#FFFFFF"),
    historic = makeAwesomeIcon(icon = "bank", library = "fa",
                               markerColor = "darkblue", iconColor = "#FFFFFF"),
    hospital = makeAwesomeIcon(icon = "h-square", library = "fa",
                               markerColor = "red", iconColor = "#FFFFFF"),
    hotels = makeAwesomeIcon(icon = "concierge-bell", library = "fa", 
                             markerColor = "green", iconColor = "#FFFFFF"),
    station = makeAwesomeIcon(icon = "subway", library = "fa", 
                              markerColor = "black", iconColor = "#FFFFFF"),
    taxi = makeAwesomeIcon(icon = "taxi", library = "fa", 
                           markerColor = "lightgray", iconColor = "#FFFFFF"),
    tourist = makeAwesomeIcon(icon = "camera", library = "fa", 
                              markerColor = "blue", iconColor = "#FFFFFF")
  )
  
  plotLocations <- function(viewOption) {
    m <- leaflet() %>% setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>% addTiles()

    if ("Bus Stops" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = busstops, lng = ~LONGITUDE, lat = ~LATITUDE, 
                                   icon = ~map_icons["busstop"], popup = ~LOC_DESC)
    }
    
    if ("Clinics" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = clinics, lng = ~LONGITUDE, lat = ~LATITUDE, 
                                   icon = ~map_icons["clinic"], popup = ~HCI_NAME)
    }
    
    if ("Hawker Centres" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = hawker_centres, lng = ~LONGITUDE, 
                                   lat = ~LATITUDE, icon = ~map_icons["hawker"], 
                                   popup = ~NAME)
    }
    
    if ("Historic Sites" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = historic_sites, lng = ~LONGITUDE, lat = ~LATITUDE, 
                                   icon = ~map_icons["historic"], popup = ~NAME)
    }
    
    if ("Hospitals" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = hospitals, lng = ~lon, lat = ~lat, 
                                   icon = ~map_icons["hospital"], popup = ~Name)
    }
    
    if ("Hotels" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = hotels, lng = ~LONGITUDE, lat = ~LATITUDE, 
                                   icon = ~map_icons["hotel"], popup = ~NAME)
    }
    
    if ("MRT/LRT Stations" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = stations, lng = ~LONGITUDE, lat = ~LATITUDE, 
                                   icon = ~map_icons["station"], popup = ~STN_NAME)
    }
    
    if ("Taxi Stands" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = taxi_stand, lng = ~Longitude, lat = ~Latitude, 
                                   icon = ~map_icons["taxi"], popup = ~Type)
    }
    
    if ("Travel Attractions" %in% viewOption) {
      m <- m %>% addAwesomeMarkers(data = attractions, lng = ~Longitude, lat = ~Latitude, 
                                   icon = ~map_icons["tourist"], popup = ~NAME)
    }
    
    m
  }
  
  output$plotLocations <- renderLeaflet({
    plotLocations(c(input$transportOptions, input$medicalOptions, input$touristOptions))
  })
}

shinyApp(ui=ui, server=server)