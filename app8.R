library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(scales)
library(leaflet.extras)

data = read_csv("food_clean_final_temp.csv")

data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
data$tonnes <- as.numeric(data$tonnes)
#Ui code
# Cree una interfaz de usuario básica con un mapa y un control de año y producto

ui <- fluidPage(
  titlePanel("World Map"),
  
  mainPanel(leafletOutput(outputId = "map", width = "100%", height = "500px"),
            plotOutput(outputId = "barplot", width = "100%", height = "200px"),
            
            sliderInput("time", "Time", min(data$Time),
                        max(data$Time), step = 1, 
                        value = c(min(data$Time), max(data$Time))),
            
            selectInput("item", "Item", unique(data$Item))
            
  ))


server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data %>%
      filter(Time >= input$time[1] & Time <= input$time[2],
             Item == input$item)
  })
  #create the map
  
  output$map <- renderLeaflet({
    colors <- colorRampPalette(c("red", "blue"))(10)
    scaled_tonnes <- rescale(filtered_data()$tonnes, c(2,10))
    
    leaflet() %>% addProviderTiles("OpenStreetMap.Mapnik") %>% 
      addCircleMarkers(data = filtered_data(), 
                       radius = scaled_tonnes,
                       color = colors[as.numeric(cut(filtered_data()$temp_change,breaks = 10))],
                       fillOpacity = 1,
                       group = "Markers")
  })
  
  
  output$barplot <- renderPlot({
    top5_countries <- filtered_data() %>% group_by(Area) %>% summarize(tonnes=sum(tonnes)) %>% top_n(5, tonnes)
    
    ggplot(top5_countries, aes(x=Area, y=tonnes)) + 
      geom_bar(stat="identity", fill="darkblue") + 
      ggtitle("Top 5 countries by tonnes") + 
      xlab("Country") + 
      ylab("Tonnes")
    
  })
  
}

shinyApp(ui, server)
