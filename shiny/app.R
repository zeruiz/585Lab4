library(shiny)
require(ggplot2)
require(dplyr)
require(leaflet)
require(plotly)


# read in data
load(file = "subdat.rda")

ui <- fluidPage(
  # App Title
  titlePanel("Story County Liquor Sales"),
  
  # Sidebar #1 - date
  sidebarPanel(
      selectInput("Year", label = ("Year"),
                  choices = sort(unique(subdat$Year)),
                  selected = 2012)
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Total Sales", plotOutput(outputId = "sales")),
      tabPanel("Sales by City", plotOutput(outputId = "cities")),
      tabPanel("Store Location", leafletOutput("map1")),
      tabPanel("Store Income heat map", plotlyOutput("map2"))
      )
    )
  )

# server
server <- function(input, output){
  liq_subset <- reactive({
    subdat %>%
      filter(Year == input$Year)
  })
  city_group <- reactive({
    subdat %>%
      filter(Year == input$Year)%>%
      group_by(City, Month)%>%
      summarize(totalSales = sum(SaleDollars))
  })
  output$sales <- renderPlot({
    ggplot(data = liq_subset(), aes(x = Month, y = SaleDollars))+
      geom_col()+
      theme_bw()+
      ggtitle(paste("Total alcohol in Story County in", input$Year))
  })
  output$cities <- renderPlot({
    ggplot(data = city_group(), aes(x = Month, y = totalSales, color = City, group = City))+
      geom_point()+
      geom_line()+
      theme_bw()+
      ggtitle(paste("Total alochol sales by city in Story Count in", input$Year))
  })
  city_year <- reactive({
    subdat %>%
      filter(Year == input$Year) %>% 
      group_by(StoreName, Lon, Lat) %>% 
      summarise(income = sum(SaleDollars))
  })
  output$map1 <- renderLeaflet({
    leaflet(data = city_year()) %>% 
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, label = ~ StoreName,
                                clusterOptions = markerClusterOptions())
  })
  output$map2 <- renderPlotly({
    p <- ggplot(data = city_year(), aes(x = Lon, y = Lat, colour = income, size=StoreName)) + 
      geom_point()+theme_bw()+theme(legend.position=c(0.8, 0.8))
    ggplotly(p)
  })
}

shinyApp(ui, server)