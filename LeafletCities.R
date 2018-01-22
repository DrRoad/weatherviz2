#install.packages('leaflet')

require(leaflet)
require(shiny)

load('data.RData')
citydata <- unique(big[,c('city','longitude','latitude','state')])


ui <- fluidPage(
  leafletOutput("map"), textOutput('text')
)


server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet(data = citydata) %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
      addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "Weather data © 2012 IEM Nexrad") %>% 
      addMarkers(~longitude, ~latitude, layerId = ~as.character(city), label = ~as.character(city))
  
    })
  observe({
    
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText({
      paste0('Selected City: ',click$id,', ',citydata$state[which(citydata$city == click$id)[1]])
    })
    
  })
}

shinyApp(ui, server)
