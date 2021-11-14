# Libraries
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(osrm)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(tm)
library(wordcloud)

# Load location data
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

# Load Tripadvisor review data
freq_word <- read.csv("./datasets/tripadvisor/ReviewWords.csv", stringsAsFactors = F)
sentiment <- read.csv("./datasets/tripadvisor/Sentiments.csv", stringsAsFactors = F)
reviews <- read.csv("./datasets/tripadvisor/Reviews.csv", stringsAsFactors = F)


ui <- dashboardPage(
  dashboardHeader(title = "Discover Singapore"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Map View", tabName = "mapview", icon = icon("map")),
      menuItem(text = "Tripadvisor Reviews", tabName = "tripadvisor", icon = icon("binoculars"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Map view tab
      tabItem(tabName = "mapview",
        sidebarLayout(
          position = "right",
          sidebarPanel(
            uiOutput(
              outputId = "select"
            ),
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
            p(strong("Instructions:"), br(),
              "1. Select one of the options under Transportation, Medical or Tourism to view all", 
              " available locations within the categories.", br(), 
              "2. Click on a cluster to have a zoomed in view of the markers within the cluster", br(),
              "3. Click on a marker to view additional information of the selected location", br(),
              "4. To view a route between two locations, repeat step 1 and select a Starting Point",
              " and Destination from the drop down list or type out your desired location", 
              style = "padding: 5px 20px 0px;"
            ),
            leafletOutput(outputId = "map"),
            fluidRow(
              column(12, offset = 0, style = "padding: 20px;", uiOutput(outputId = "information"))
            )
          )
        )
      ),
      
      # Tripadvisor tab
      tabItem(tabName = "tripadvisor",
        sidebarLayout(
          position = "right",
          sidebarPanel(
            selectInput("selectAttraction", label = h3("Select Attraction"), 
                        choices = sort(unique(reviews$attraction_name))
            ),
            p(strong("Instructions:"), br(),
              "1. Select the travel attraction in the drop down bar", br(),
              "2. View the wordcloud to see what others are saying about the place", br(),
              "3. View the sentiment analysis", br(),
              "4. View reviews from other users who visited the attraction"
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Sentiments", 
                       br(),
                       h4("Visitors' sentiments"),
                       plotOutput("SentimentAnalysis"),
              ), 
              tabPanel("Wordcloud",
                       br(),
                       h4("Visitors' Word"),
                       plotOutput("FreqCloud"),
              ), 
              tabPanel("Full Review", 
                       br(),
                       h4("Visitors' Review"),
                       dataTableOutput("ReviewsTable")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output){
  # Map view tab
  # Filters combined data frame by checkbox selection
  filtered_data <- reactive({
    viewOptions <- c(input$transportOptions, input$medicalOptions, input$touristOptions)
    combined_data %>% filter(., Group %in% viewOptions)
  })
  
  # Parses the UI selection string into a vector of data and row indices to access
  # the original data frames
  getRowDetails <- function(selection) {
    ref <- unlist(strsplit(selection, split = "-"))
    data_index <- as.numeric(ref[1])
    row_index <- as.numeric(ref[2])
    return (c(data_index, row_index))
  }
  
  # A UI component for searching and selecting a start point and destination
  output$select <- renderUI({
    df <- filtered_data()
    verticalLayout(
      selectizeInput(
        inputId = "startPoint",
        label = "Starting Point:",
        choices = setNames(with(df, paste0(Group, "-", Row_ID)), df$Name), 
        multiple = T,
        options = list(maxItems = 1)
      ),
      selectizeInput(
        inputId = "destination",
        label = "Destination:",
        choices = setNames(with(df, paste0(Group, "-", Row_ID)), df$Name), 
        multiple = T,
        options = list(maxItems = 1)
      )
    )
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
  
  # Plots a route between two selected locations
  plotRoute <- function(m) {
    # Checks if there are no selections or NA is selected in selectizeInput component
    if (!is.null(input$startPoint) && input$startPoint != "-" && 
        !is.null(input$destination) && input$destination != "-") {
      start_ref <- getRowDetails(input$startPoint)
      dest_ref <- getRowDetails(input$destination)
      df <- filtered_data()

      start_df <- df %>% filter(Group == start_ref[1], Row_ID == start_ref[2])
      dest_df <- df %>% filter(Group == dest_ref[1], Row_ID == dest_ref[2])
      
      # Get vectors of longitude and latitude of start point and destination
      start_coords <- start_df %>% select(Longitude, Latitude) %>% as.numeric()
      dest_coords <- dest_df %>% select(Longitude, Latitude) %>% as.numeric()
      
      # Longitudes and Latitudes of entire route between start point and destination
      route = osrmRoute(start_coords, dest_coords, overview = "full")
      
      # Get duration and distance of route
      route_info = osrmRoute(start_coords, dest_coords, overview = F)
      
      hours = route_info[1]/60
      mins = round(ifelse(route_info[1] < 1, (hours%%1) * 60, route_info[1]))
      
      # Plot 2 markers and route between them
      m %>% addAwesomeMarkers(data = start_df, lng = ~Longitude, lat = ~Latitude, 
                              layerId = ~paste0(Group, "-", Row_ID), icon = ~map_icons[start_ref[1]], 
                              popup = ~Name) %>%
        addAwesomeMarkers(data = dest_df, lng = ~Longitude, lat = ~Latitude, 
                          layerId = ~paste0(Group, "-", Row_ID), icon = ~map_icons[dest_ref[1]], 
                          popup = ~Name) %>%
        addPolylines(route$lon, route$lat, popup = paste(ifelse(hours > 1, paste(round(hours), "hrs"), ""),
                                                         mins, "mins", br(), round(route_info[2]), "km"))
    } else {
      m
    }
  }
  
  output$map <- renderLeaflet({
    # Set map view to Singapore's coordinates
    leaflet() %>% addTiles() %>% setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  # Retains zoom view of map when user selects additional options
  observe({
    leafletProxy("map") %>% clearMarkerClusters() %>% clearShapes() %>% plotLocations() %>% plotRoute()
  })
  
  # Observes the leaflet marker that the user clicks on
  observeEvent(input$map_marker_click, {
    ref <- getRowDetails(input$map_marker_click[[1]])
    data_index <- ref[1]
    row_index <- ref[2]
    
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
          p(strong("Name: "), selected_row[2], br(),
            strong("Type: "), selected_row[3], br(),
            strong("Longitude: "), selected_row[4], br(),
            strong("Latitude: "), selected_row[5], br(),
            strong("Address: "), selected_row[6]
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
  
  # Tripadvisor Tab
  # Sentiment Analysis
  sentiment_df <- reactive({
    sentiment_df1 <- sentiment[sentiment$attraction_name == input$selectAttraction,]
    sentiment_df1
  })
  
  output$SentimentAnalysis <- renderPlot(
    ggplot(data=sentiment_df(), aes(x = sentiment, y = score)) +
      geom_bar(aes(fill = sentiment),stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("Scores")+
      theme_minimal()
  )
  
  # Wordcloud
  wordcloud_df <- reactive({
    wordcloud_df1 <- freq_word[freq_word$attraction_name == input$selectAttraction,]$word
    wordcloud_df1
  })
  
  output$FreqCloud <- renderPlot(
    wordcloud(wordcloud_df(), min.freq = 5, scale=c(4.5, .5), random.order = FALSE, rot.per = 0.35, 
              max.words = 200, colors = brewer.pal(8, "Dark2")))
  
  #Reviews Table
  reviews_df <- reactive({
    reviews_df1 <- reviews[reviews$attraction_name == input$selectAttraction,] %>% select(3,4)
    names(reviews_df1) <- c("Reviews","Review Date")
    reviews_df1
  })
  
  output$ReviewsTable <- DT::renderDataTable(
    DT::datatable(reviews_df(), options = list(searching = FALSE),rownames= FALSE),
  )
}

shinyApp(ui = ui, server = server)
