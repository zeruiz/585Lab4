library(shiny)
require(ggplot2)
require(dplyr)

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
      tabPanel("Sales by City", plotOutput(outputId = "cities"))
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
}

shinyApp(ui, server)