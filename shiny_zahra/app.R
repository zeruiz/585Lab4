library(shiny)
library(tidyverse)
library(leaflet)


# load data
load(file = "clean_data.rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Liquor Consumption in Story County!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput("Year", label = ("Select Year"),
                  choices = sort(unique(sub_data$Year)),
                  selected = 2012),
      selectInput("city", label = ("Select city"),
                  choices = sort(unique(sub_data$city)),
                  selected = "ames")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("consumption by City in Year", plotOutput(outputId = "consplotyear")),
        tabPanel("Consumption of City in different year",  plotOutput(outputId = "consplotcity")),
        tabPanel("Spatial plot", leaflet::leafletOutput("map"))
      )
      
       )
    
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  liq_subset <- reactive({
    sub_data %>%
      filter(Year == input$Year)
  })
  
  liqcity_subset <- reactive({
    sub_data %>%
      filter(city == input$city)
  })
  
  output$consplotyear <- renderPlot({
    ggplot(data = liq_subset(), aes(x = city, y = VolumeSoldLiters,color=zipcode,group=zipcode))+
      geom_col()+
      ggtitle(paste("Total alcohol consumption in Story County in", input$Year))

    
  })
  
  output$consplotcity <- renderPlot({
    ggplot(data = liqcity_subset(), aes(x = Year, y = VolumeSoldLiters,color = DayOfWeek,group=DayOfWeek))+
      geom_col()+
      ggtitle(paste("Total alcohol consumption of", input$city))
    
    
  })
  
  city_year <- reactive({
    sub_data %>%
      filter(Year == input$Year) %>% 
      group_by(zipcode, lon, lat) %>% 
      summarise(consumption = sum(VolumeSoldLiters))
  })
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(data = city_year()) %>% leaflet::addTiles() %>%
      leaflet::addCircleMarkers(~lon, ~lat, label = ~ zipcode,
                                clusterOptions = markerClusterOptions())
  })
  
}

shinyApp(ui = ui, server = server)