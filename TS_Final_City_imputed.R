city <- read.csv("~/Desktop/Time Series/TS FP Raw Data/GlobalLandTemperaturesByMajorCity.csv")
unique(city["City"])

library(tidyverse)
lessna <- city %>% group_by(City)%>% summarize(sumNA = sum(is.na(AverageTemperature)))
number <- city %>% group_by(City)%>% summarize(quantity = n())

print(number[order(number$quantity,decreasing=TRUE),],n=100)
number[number["City"]=="Shanghai",]
number[number["City"]=="Seoul",]
number[number["City"]=="Mexico",]
number[number["City"]=="Los Angeles",]
number[number["City"]=="London",]

print(lessna[order(lessna$sumNA,decreasing=FALSE),],n=100)
lessna[order(lessna$sumNA,decreasing=FALSE),]
lessna[lessna["City"] == "Berlin",]
lessna[lessna["City"] == "Chicago",]
lessna[lessna["City"] == "Istanbul",]
lessna[lessna["City"] == "Kiev",]
lessna[lessna["City"] == "London",]
lessna[lessna["City"] == "Madrid",]
lessna[lessna["City"] == "Montreal",]
lessna[lessna["City"] == "Moscow",]
lessna[lessna["City"] == "Paris",]
lessna[lessna["City"] == "Rome",]


# Select Shanghai Seoul Berlin London Cairo Melbourne Los Angeles Mexico
start_dt <- c(city[city["City"] == "Shanghai",]$dt[1],
              city[city["City"] == "Seoul",]$dt[1],
              city[city["City"] == "Berlin",]$dt[1],
              city[city["City"] == "London",]$dt[1],
              city[city["City"] == "Los Angeles",]$dt[1],
              city[city["City"] == "Mexico",]$dt[1],
              city[city["City"] == "Cairo",]$dt[1],
              city[city["City"] == "Melbourne",]$dt[1])
sum_na <- c(lessna[lessna["City"] == "Shanghai",]$sumNA,
            lessna[lessna["City"] == "Seoul",]$sumNA,
            lessna[lessna["City"] == "Berlin",]$sumNA,
            lessna[lessna["City"] == "London",]$sumNA,
            lessna[lessna["City"] == "Los Angeles",]$sumNA,
            lessna[lessna["City"] == "Mexico",]$sumNA,
            lessna[lessna["City"] == "Cairo",]$sumNA,
            lessna[lessna["City"] == "Melbourne",]$sumNA)
cities <- c("Shanghai","Seoul","Berlin","London","Los Angeles","Mexico","Cairo","Melbourne")
view(data.frame(Cities=cities, Sum_NA=sum_na, Start_Date=start_dt))


shanghai <- city[city["City"] == "Shanghai",]
seoul <- city[city["City"] == "Seoul",]

library(xts)
shanghai$dt <- as.yearmon(shanghai$dt, "%Y-%m-%d")
seoul$dt <- as.yearmon(seoul$dt, "%Y-%m-%d")
shanghai <- xts(shanghai$AverageTemperature,order.by = shanghai$dt)
seoul <- xts(seoul$AverageTemperature,order.by = seoul$dt)

shanghai <- window(shanghai,start = "1848-12-01 01:00:00")
seoul <- window(seoul,start = "1848-12-01 01:00:00")

library(imputeTS)
statsNA(shanghai)
shanghai <- na_kalman(shanghai)
statsNA(shanghai)
write.zoo(shanghai, file = '~/Desktop/TS final project/Cities/shanghai.csv', sep = ',')

statsNA(seoul)
seoul <- na_kalman(seoul)
statsNA(seoul)
write.zoo(seoul, file = '~/Desktop/TS final project/Cities/seoul.csv', sep = ',')


berlin <- city[city["City"] == "Berlin",]
berlin$dt <- as.yearmon(berlin$dt, "%Y-%m-%d")
berlin <- xts(berlin$AverageTemperature,order.by = berlin$dt)
berlin <- window(berlin,start = "1848-12-01 01:00:00")
statsNA(berlin)
berlin <- na_kalman(berlin)
statsNA(berlin)
write.zoo(berlin, file = '~/Desktop/TS final project/Cities/berlin.csv', sep = ',')

london <- city[city["City"] == "London",]
london$dt <- as.yearmon(london$dt, "%Y-%m-%d")
london <- xts(london$AverageTemperature,order.by = london$dt)
london <- window(london,start = "1848-12-01 01:00:00")
statsNA(london)
london <- na_kalman(london)
statsNA(london)
write.zoo(london, file = '~/Desktop/TS final project/Cities/london.csv', sep = ',')

cairo <- city[city["City"] == "Cairo",]
cairo$dt <- as.yearmon(cairo$dt, "%Y-%m-%d")
cairo <- xts(cairo$AverageTemperature,order.by = cairo$dt)
cairo <- window(cairo,start = "1848-12-01 01:00:00")
statsNA(cairo)
cairo <- na_kalman(cairo)
statsNA(cairo)
write.zoo(cairo, file = '~/Desktop/TS final project/Cities/cairo.csv', sep = ',')

melbourne <- city[city["City"] == "Melbourne",]
melbourne$dt <- as.yearmon(melbourne$dt, "%Y-%m-%d")
melbourne <- xts(melbourne$AverageTemperature,order.by = melbourne$dt)
melbourne <- window(melbourne,start = "1848-12-01 01:00:00")
statsNA(melbourne)
melbourne <- na_kalman(melbourne)
statsNA(melbourne)
write.zoo(melbourne, file = '~/Desktop/TS final project/Cities/melbourne.csv', sep = ',')

los_angeles <- city[city["City"] == "Los Angeles",]
los_angeles$dt <- as.yearmon(los_angeles$dt, "%Y-%m-%d")
los_angeles <- xts(los_angeles$AverageTemperature,order.by = los_angeles$dt)
los_angeles <- window(los_angeles,start = "1848-12-01 01:00:00")
statsNA(los_angeles)
los_angeles <- na_kalman(los_angeles)
statsNA(los_angeles)
write.zoo(los_angeles, file = '~/Desktop/TS final project/Cities/los_angeles.csv', sep = ',')

mexico <- city[city["City"] == "Mexico",]
mexico$dt <- as.yearmon(mexico$dt, "%Y-%m-%d")
mexico <- xts(mexico$AverageTemperature,order.by = mexico$dt)
mexico <- window(mexico,start = "1848-12-01 01:00:00")
statsNA(mexico)
mexico <- na_kalman(mexico)
statsNA(mexico)
write.zoo(mexico, file = '~/Desktop/TS final project/Cities/mexico.csv', sep = ',')