global <- read.csv("~/downloads/TS_Climate/GlobalTemperatures.csv")
class(global$dt)
class(global$LandAverageTemperature)

library(xts)
library(tidyverse)
global$dt <- as.yearmon(global$dt, "%Y-%m-%d")
class(global$dt)
global_xts <- xts(global$LandAverageTemperature, order.by = global$dt)
names(global_xts) <- 'LandAverageTemperature'
ts.plot(global_xts)

library(imputeTS)
statsNA(global_xts)
newglobal_xts <- na_kalman(global_xts)
statsNA(newglobal_xts)
#write.zoo(newglobal_xts, file = '~/downloads/imputed_Global.csv', sep = ',')

train <- global_xts[c(1:3156),]
test <- global_xts[c(3157:3192),]

library(forecast)
sarima_temp <- auto.arima(train, D=1)
summary(sarima_temp)

sarima_pred <- forecast(sarima_temp, h=36)
sarima_pred$mean

library(Metrics)
rmse <- rmse(test$LandAverageTemperature, sarima_pred$mean)
smape <- smape(test$LandAverageTemperature, sarima_pred$mean)
error <- c(rmse, smape)
error

city <- read.csv("~/downloads/TS_Climate/GlobalLandTemperaturesByMajorCity.csv")
city$dt <- as.yearmon(city$dt, "%Y-%m-%d")
class(city$dt)

berlin <- read.csv("~/downloads/TS_Data/berlin.csv", header = FALSE)
cairo <- read.csv("~/downloads/TS_Data/cairo.csv", header = FALSE)
la <- read.csv("~/downloads/TS_Data/los_angeles.csv", header = FALSE)
mel <- read.csv("~/downloads/TS_Data/melbourne.csv", header = FALSE)
mexico <- read.csv("~/downloads/TS_Data/mexico.csv", header = FALSE)
seoul <- read.csv("~/downloads/TS_Data/seoul.csv", header = FALSE)
shanghai <- read.csv("~/downloads/TS_Data/shanghai.csv", header = FALSE)
london <- read.csv("~/downloads/TS_Data/london.csv", header = FALSE)

class(berlin$V1)

berlin$V1 <- as.yearmon(berlin$V1, "%b %Y")
class(berlin$V1)
cairo$V1 <- as.yearmon(cairo$V1, "%b %Y")
la$V1 <- as.yearmon(la$V1, "%b %Y")
mel$V1 <- as.yearmon(mel$V1, "%b %Y")
mexico$V1 <- as.yearmon(mexico$V1, "%b %Y")
seoul$V1 <- as.yearmon(seoul$V1, "%b %Y")
shanghai$V1 <- as.yearmon(shanghai$V1, "%b %Y")
london$V1 <- as.yearmon(london$V1, "%b %Y")

berlin_xts <- xts(berlin$V2, order.by = berlin$V1)
cairo_xts <- xts(cairo$V2, order.by = cairo$V1)
la_xts <- xts(la$V2, order.by = la$V1)
mel_xts <- xts(mel$V2, order.by = mel$V1)
mexico_xts <- xts(mexico$V2, order.by = mexico$V1)
seoul_xts <- xts(seoul$V2, order.by = seoul$V1)
shanghai_xts <- xts(shanghai$V2, order.by = shanghai$V1)
london_xts <- xts(london$V2, order.by = london$V1)

berlin_train <- berlin_xts[c(1:1941),]
berlin_test <- berlin_xts[c(1942:1977),]
cairo_train <- cairo_xts[c(1:1941),]
cairo_test <- cairo_xts[c(1942:1977),]
la_train <- la_xts[c(1:1941),]
la_test <- la_xts[c(1942:1977),]
mel_train <- mel_xts[c(1:1941),]
mel_test <- mel_xts[c(1942:1977),]
mexico_train <- mexico_xts[c(1:1941),]
mexico_test <- mexico_xts[c(1942:1977),]
seoul_train <- seoul_xts[c(1:1941),]
seoul_test <- seoul_xts[c(1942:1977),]
shanghai_train <- shanghai_xts[c(1:1941),]
shanghai_test <- shanghai_xts[c(1942:1977),]
london_train <- london_xts[c(1:1941),]
london_test <- london_xts[c(1942:1977),]

sarima_berlin <- auto.arima(berlin_train, D=1)
summary(sarima_berlin)
sarima_cairo <- auto.arima(cairo_train, D=1)
summary(sarima_cairo)
sarima_la <- auto.arima(la_train, D=1)
summary(sarima_la)
sarima_mel <- auto.arima(mel_train, D=1)
summary(sarima_mel)
sarima_mexico <- auto.arima(mexico_train, D=1)
summary(sarima_mexico)
sarima_seoul <- auto.arima(seoul_train, D=1)
summary(sarima_seoul)
sarima_shanghai <- auto.arima(shanghai_train, D=1)
summary(sarima_shanghai)
sarima_london <- auto.arima(london_train, D=1)
summary(sarima_london)

berlin_pred <- forecast(sarima_berlin, h=36)
berlin_rmse <- rmse(berlin_test, berlin_pred$mean)
berlin_smape <- smape(berlin_test, berlin_pred$mean)

cairo_pred <- forecast(sarima_cairo, h=36)
cairo_rmse <- rmse(cairo_test, cairo_pred$mean)
cairo_smape <- smape(cairo_test, cairo_pred$mean)

la_pred <- forecast(sarima_la, h=36)
la_rmse <- rmse(la_test, la_pred$mean)
la_smape <- smape(la_test, la_pred$mean)

mel_pred <- forecast(sarima_mel, h=36)
mel_rmse <- rmse(mel_test, mel_pred$mean)
mel_smape <- smape(mel_test, mel_pred$mean)

mexico_pred <- forecast(sarima_mexico, h=36)
mexico_rmse <- rmse(mexico_test, mexico_pred$mean)
mexico_smape <- smape(mexico_test, mexico_pred$mean)

seoul_pred <- forecast(sarima_seoul, h=36)
seoul_rmse <- rmse(seoul_test, seoul_pred$mean)
seoul_smape <- smape(seoul_test, seoul_pred$mean)

shanghai_pred <- forecast(sarima_shanghai, h=36)
shanghai_rmse <- rmse(shanghai_test, shanghai_pred$mean)
shanghai_smape <- smape(shanghai_test, shanghai_pred$mean)

london_pred <- forecast(sarima_london, h=36)
london_rmse <- rmse(london_test, london_pred$mean)
london_smape <- smape(london_test, london_pred$mean)

city <- c('Berlin', 'Cairo', 'Los Angeles', 'Melbourne', 'Mexico', 'Seoul', 'Shanghai', 'London')
rmse <- c(berlin_rmse, cairo_rmse, la_rmse, mel_rmse, mexico_rmse, seoul_rmse, shanghai_rmse, london_rmse)
smape <- c(berlin_smape, cairo_smape, la_smape, mel_smape, mexico_smape, seoul_smape, shanghai_smape, london_smape)

data.frame(City = city, RMSE = rmse, sMAPE = smape)
