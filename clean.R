require(dplyr) #install.packages('dplyr') if not available
require(tidyr)  #install.packages('tidyr') if not available
require(lubridate) #install.packages('lubridate') if not available

loc <- read.csv('locations.csv')
loc$CityIndex <- 1:nrow(loc)
actual <- read.csv('histWeather.csv')
actual <- left_join(actual,loc,by = c('AirPtCd')) #info from loc and actual are now merged into actual
actual$Date <- ymd(actual$Date)
actual$PrecipitationIn <- as.numeric(as.character(actual$PrecipitationIn))
actual$Max_TemperatureF[which(actual$Max_TemperatureF > 200)] <- NA #error - make it missing
actual$Min_TemperatureF[which(actual$Min_TemperatureF < -100)] <- NA #error - make it missing
actual$Mean_TemperatureF[which(actual$Mean_TemperatureF < -100)] <- NA #error - make it missing
#you may find more errors as you explore the data


forecast <- read.delim('forecast.dat',sep=' ',header=FALSE)
names(forecast) <- c('CityIndex','Date','Value','Variable','DateForecasted')

###Following code is to Deal with multiple forecast values for the same date##
forecast$id = paste0(forecast$Date,'-',forecast$CityIndex,'-',forecast$DateForecasted)
forecast$Variable <- as.character(forecast$Variable)
forecast <- forecast[order(forecast$id),]
foo <- unlist(tapply(forecast$Variable,forecast$id,function(v){ 
  j = which(v == 'MinTemp'); if(length(j) > 1) v[j[-1]] = 'MinTemp2'
  j = which(v == 'MaxTemp'); if(length(j) > 1) v[j[-1]] = 'MaxTemp2'
  j = which(v == 'ProbPrecip'); if(length(j) > 1) v[j[-1]] = 'ProbPrecip2'
  j = which(v == 'ProbPrecip2'); if(length(j) > 1) v[j[-1]] = 'ProbPrecip3'
  return(v)}))
forecast$Variable <- foo
forecast <- forecast[forecast$Variable %in% c('MinTemp','MaxTemp','ProbPrecip','ProbPrecip2'),]
##

forecast <- spread(forecast,Variable,Value) #reshaped the data set
names(forecast)[5:8] <- paste0('F',names(forecast)[5:8]) #changed the names to emphasize forecasted values
forecast$Date <- ymd(forecast$Date) #Converted the string to a date variable
forecast$DateForecasted <- ymd(forecast$DateForecasted) #Converted the string to a date variable
forecast$DateDiff = as.numeric(forecast$Date - forecast$DateForecasted) #How far out the forecast is

forecast <- forecast[forecast$DateDiff > -1,] #remove rows with forecasts after the date
forecast[,5:8] <- apply(forecast[,5:8],2,as.numeric)



#Reshape to (really) Wide Format
forecastwide <- forecast[,-(3:4)]
forecastwide <- forecastwide %>% 
  gather(key, val, 3:6) %>% 
  unite(key2, key,DateDiff, sep = "_Days") %>% 
  spread(key2, val) 



