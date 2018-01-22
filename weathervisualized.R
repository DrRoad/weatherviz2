#install.packages('dplyr') #only once
#install.packages('tidyr') #only once
#install.packages('lubridate') #only once
#install.packages('noncensus')

#source('clean.R') #takes a second to run
#big <- left_join(actual,forecastwide)
#big <- big[order(big$Date),]
#head(big,10)

#require(noncensus) #install.packages('noncensus') once to install
#data(state)
#foo = data.frame(state=state.name,div=state.division,region=state.region)
#big <- left_join(big,foo)
#save(big,actual,forecast,forecastwide,file='data.RData')


load('data.RData')

require(leaflet)
require(shiny)
require(ggplot2)

load('data.RData')
citydata <- unique(big[,c('CityIndex','city','longitude','latitude','state')])

# format the page
ui <- fluidPage(
        titlePanel("Weather Data Visualization"),
        fluidRow(
                # drop down menu to select metrics
                column(4, wellPanel(
                        selectInput(
                                "metric_type", "Weather Metrics", 
                                c("Temperature", "Humidity", "Precipitation", "Dew Point",
                                  "Sea Level Pressure", "Visibility", "Wind Speed") 
                        )
                )),
                # map to select cities
                column(8, wellPanel(
                        leafletOutput("map"), textOutput('text')
                ))
        ),
        fluidRow(
                # check boxes to specify which graphs are displayed
                column(4, wellPanel(
                        uiOutput("radioA"),uiOutput("radioF")
                )),
                
                # dispayed graphs for the city
                column(8, wellPanel(
                        plotOutput('graphs')
                ))
        )
)


server <- function(input, output) {
        
        output$map <- renderLeaflet({
                
                leaflet(data = citydata) %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
                        addWMSTiles(
                                "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                                layers = "nexrad-n0r-900913",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad") %>% 
                        addMarkers(~longitude, ~latitude, layerId = ~as.character(CityIndex), label = ~as.character(city))
                
        })
        
        observe({
                
                
                
                click <- input$map_marker_click
                if(is.null(click))
                        return()
                output$text <- renderText({
                        paste0('Selected City: ',citydata$city[which(citydata$CityIndex == click$id)[1]],
                               ', ',citydata$state[which(citydata$CityIndex == click$id)[1]])
                })
                output$graphs <- renderPlot({
                        
                        # sets the y axis label
                        yaxlab <- switch(input$metric_type,
                                         "Temperature" = "Temperature (ºF)",
                                         "Humidity" = "Humidity (%)",
                                         "Precipitation" = if(input$var3 == "option1") {
                                                                return("Precipitation (in)")
                                                                } else {
                                                                        return("Precipitation (% Chance)")
                                                                },
                                         "Dew Point" = "Dew Point",
                                         "Sea Level Pressure" = "Sea Level Pressure",
                                         "Visibility" = "Visibility (miles)",
                                         "Wind Speed" = "Wind Speed (miles / hour)"
                                         
                                         )
                        # sets the title
                        graphtitle <- switch(input$metric_type,
                                         "Temperature" = paste0("Temperature in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                                ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]),
                                         "Humidity" = paste0("Humidity in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                         ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]),
                                         "Precipitation" = if(input$var3 == "option1") {
                                                 return(paste0("Precipitation in", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                        ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]))
                                         } else {
                                                 return(paste0("Chance of Precipitation in", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                               ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]))
                                         },
                                         "Dew Point" = paste0("Dew Point in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                              ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]),
                                         "Sea Level Pressure" = paste0("Sea Level Pressure in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                                       ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]),
                                         "Visibility" = paste0("Visibility in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                               ', ',citydata$state[which(citydata$CityIndex == click$id)[1]]),
                                         "Wind Speed" = paste0("Wind Speed in ", citydata$city[which(citydata$CityIndex == click$id)[1]],
                                                               ', ',citydata$state[which(citydata$CityIndex == click$id)[1]])
                                         
                        )
                        
                        #sets the legend labels
                        actuallegendlabel <- switch(input$metric_type, 
                                               "Temperature" = switch(input$actualtemp,
                                                                      "option1" = "Max Temperature",
                                                                      "option2" = "Mean Temperature",
                                                                      "option3" = "Min Temperature"
                                               ),
                                               "Humidity" = switch(input$var2,
                                                                   "option1" = "Max Humidity",
                                                                   "option2" = "Mean Humidity",
                                                                   "option3" = "Min Humidity"
                                               ),
                                               "Precipitation" = switch(input$var3,
                                                                        "option1" = "Actual Precipitation",
                                                                        "option2" = "Chance of Precipitation: 0",
                                                                        "option3" = "Chance of Precipitation: 1",
                                                                        "option4" = "Chance of Precipitation: 2",
                                                                        "option5" = "Chance of Precipitation: 3",
                                                                        "option6" = "Chance of Precipitation: 4",
                                                                        "option7" = "Chance of Precipitation: 5",
                                                                        "option8" = "Chance of Precipitation: 6",
                                                                        "option9" = "Chance of Precipitation: 7"
                                               ),
                                               "Dew Point" = switch(input$var4,
                                                                    "option1" = "Max Dew Point",
                                                                    "option2" = "Mean Dew Point",
                                                                    "option3" = "Min Dew Point"
                                               ),
                                               "Sea Level Pressure" = switch(input$var5,
                                                                             "option1" = "Max Sea Level Pressure",
                                                                             "option2" = "Mean Sea Level Pressure",
                                                                             "option3" = "Min Sea Level Pressure"
                                               ),
                                               "Visibility" = switch(input$var6,
                                                                     "option1" = "Max Visibility",
                                                                     "option2" = "Mean Visibility",
                                                                     "option3" = "Min Visibility"
                                               ),
                                               "Wind Speed" = switch(input$var7,
                                                                     "option1" = "Max Wind Speed",
                                                                     "option2" = "Mean Wind Speed",
                                                                     "option3" = "Max Gust Speed"
                                               )
                                               
                        ) 
                        # nested switch statement to retrieve the appropriate data from selected radio buttons
                        metricchoice <- switch(input$metric_type, 
                                "Temperature" = switch(input$actualtemp,
                                                "option1" = "Max_TemperatureF",
                                                "option2" = "Mean_TemperatureF",
                                                "option3" = "Min_TemperatureF"
                                ),
                                "Humidity" = switch(input$var2,
                                                "option1" = "Max_Humidity",
                                                "option2" = "Mean_Humidity",
                                                "option3" = "Min_Humidity"
                                ),
                                "Precipitation" = switch(input$var3,
                                                "option1" = "PrecipitationIn",
                                                "option2" = "FProbPrecip_Days0",
                                                "option3" = "FProbPrecip_Days1",
                                                "option4" = "FProbPrecip_Days2",
                                                "option5" = "FProbPrecip_Days3",
                                                "option6" = "FProbPrecip_Days4",
                                                "option7" = "FProbPrecip_Days5",
                                                "option8" = "FProbPrecip_Days6",
                                                "option9" = "FProbPrecip_Days7"
                                ),
                                "Dew Point" = switch(input$var4,
                                                "option1" = "Max_Dew_PointF",
                                                "option2" = "MeanDew_PointF",
                                                "option3" = "Min_DewpointF"
                                ),
                                "Sea Level Pressure" = switch(input$var5,
                                                "option1" = "Max_Sea_Level_PressureIn",
                                                "option2" = "Mean_Sea_Level_PressureIn",
                                                "option3" = "Min_Sea_Level_PressureIn"
                                ),
                                "Visibility" = switch(input$var6,
                                                "option1" = "Max_VisibilityMiles",
                                                "option2" = "Mean_VisibilityMiles",
                                                "option3" = "Min_VisibilityMiles"
                                ),
                                "Wind Speed" = switch(input$var7,
                                                "option1" = "Max_Wind_SpeedMPH",
                                                "option2" = "Mean_Wind_SpeedMPH",
                                                "option3" = "Max_Gust_SpeedMPH"
                                )
                                               
                        ) 
                        
                        forecastchoice <- if(input$metric_type == 'Temperature') {
                                switch(input$forecasttemp,
                                       "option4" = "FMaxTemp_Days0",
                                       "option5" = "FMaxTemp_Days1",
                                       "option6" = "FMaxTemp_Days2",
                                       "option7" = "FMaxTemp_Days3",
                                       "option8" = "FMinTemp_Days0",
                                       "option9" = "FMinTemp_Days1",
                                       "option10" = "FMinTemp_Days2",
                                       "option11" = "FMinTemp_Days3"
                                )
                        }
                        
                        # remove missing values
                        metricchoice <- metricchoice[!is.na(metricchoice)]
                        
                        # ggplot graph takes city as input
                        graph <- big %>% filter(city %in% citydata$city[citydata$CityIndex == click$id]) %>%
                                ggplot(aes_string(x = "Date", y = metricchoice, col="metricchoice")) + 
                                geom_line() +
                                ylab(yaxlab) +
                                ggtitle(graphtitle) +
                                labs(caption = "by Geoff Salmon and Joe Laham") 
                                
                        
                        # adds forecasted only if Temperature is the selected metric
                        if (input$metric_type == 'Temperature') {
                                forecastchoice <- forecastchoice[!is.na(forecastchoice)]
                                forecastlegendlabel <- switch(input$forecasttemp,
                                                              "option4" = "Forecasted Max Temperature: 0",
                                                              "option5" = "Forecasted Max Temperature: 1",
                                                              "option6" = "Forecasted Max Temperature: 2",
                                                              "option7" = "Forecasted Max Temperature: 3",
                                                              "option8" = "Forecasted Min Temperature: 0",
                                                              "option9" = "Forecasted Min Temperature: 1",
                                                              "option10" = "Forecasted Min Temperature: 2",
                                                              "option11" = "Forecasted Min Temperature: 3"
                                )
                                
                                graph + geom_line(data = big %>% filter(city %in% citydata$city[citydata$CityIndex == click$id]), 
                                                  aes_string(x = "Date", y = forecastchoice, col = "forecastchoice")) +
                                        scale_color_manual(labels = c(forecastlegendlabel, actuallegendlabel), values = c("blue", "red"))
                        }
                                
                        
                })
        })
        
        # Actual weather metric radio options 
        output$radioA <- renderUI({
                switch(input$metric_type,
                        "Temperature" = radioButtons('actualtemp',label='Actual',
                                                     choices = c("Actual Max Temperature" = "option1",
                                                                 "Actual Mean Temperature" = "option2",
                                                                 "Actual Min Temperature" = "option3"
                                                    )),
                       "Humidity" = radioButtons('var2',label='Choose Variable',
                                                 choices = c("Max Humidity" = "option1",
                                                             "Mean Humidity" = "option2",
                                                             "Min Humidity" = "option3"
                                                 )), 
                       "Precipitation" = radioButtons('var3',label='Choose Variable',
                                                      choices = c("Actual Precipitation" = "option1",
                                                                  "Chance of Precipitation: 0" = "option2",
                                                                  "Chance of Precipitation: 1" = "option3",
                                                                  "Chance of Precipitation: 2" = "option4",
                                                                  "Chance of Precipitation: 3" = "option5",
                                                                  "Chance of Precipitation: 4" = "option6",
                                                                  "Chance of Precipitation: 5" = "option7",
                                                                  "Chance of Precipitation: 6" = "option8",
                                                                  "Chance of Precipitation: 7" = "option9"
                                                      )), 
                       "Dew Point" = radioButtons('var4',label='Choose Variable',
                                                  choices = c("Max Dew Point" = "option1",
                                                              "Mean Dew Point" = "option2",
                                                              "Min Dew Point" = "option3"
                                                  )), 
                       "Sea Level Pressure" = radioButtons('var5',label='Choose Variable',
                                                           choices = c("Max Sea Level Pressure" = "option1",
                                                                       "Mean Sea Level Pressure" = "option2",
                                                                       "Min Sea Level Pressure" = "option3"
                                                           )), 
                       "Visibility" = radioButtons('var6',label='Choose Variable',
                                                   choices = c("Max Visibility" = "option1",
                                                               "Mean Visibility" = "option2",
                                                               "Min Visibility" = "option3"
                                                   )), 
                       "Wind Speed" = radioButtons('var7',label='Choose Variable',
                                                   choices = c("Max Wind Speed" = "option1",
                                                               "Mean Wind Speed" = "option2",
                                                               "Max Gust Speed" = "option3"
                                                   ))
                )
                
        })
        
        # Forecasted temperature radio options
        output$radioF <- renderUI({
                if(input$metric_type == 'Temperature'){
                        radioButtons('forecasttemp', label="Forecasted",
                                     choices = c("Forecasted Max Temperature: 0" = "option4",
                                                 "Forecasted Max Temperature: 1" = "option5",
                                                 "Forecasted Max Temperature: 2" = "option6",
                                                 "Forecasted Max Temperature: 3" = "option7",
                                                 "Forecasted Min Temperature: 0" = "option8",
                                                 "Forecasted Min Temperature: 1" = "option9",
                                                 "Forecasted Min Temperature: 2" = "option10",
                                                 "Forecasted Min Temperature: 3" = "option11"
                                     ))
                }
        })
}

shinyApp(ui, server)