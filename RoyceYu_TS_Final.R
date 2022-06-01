#Global Data
global <- read.csv("~/downloads/TS_Climate/GlobalTemperatures.csv")
class(global$dt)
class(global$LandAverageTemperature)
head(global, n=5)

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

train <- newglobal_xts[c(1:3156),]
test <- newglobal_xts[c(3157:3192),]
vali <- newglobal_xts[c(1993:3192),]
decomlat = stl(train, s.window="periodic")
plot(train)
plot(vali)
plot(decomlat)
plot(test)
library(tseries)
kpss.test(train)
adf.test(train, alternative = "stationary")
acf(train,lag=100)
pacf(train,lag=100)

library(forecast)
#sarima_temp <- auto.arima(train, D=1)
#summary(sarima_temp)
#checkresiduals(sarima_temp,lag.max=11)

#sarima_pred <- forecast(sarima_temp, h=36)
#sarima_pred$mean
#autoplot(sarima_pred,include=156)

sarima_mod = Arima(train,order=c(0,1,1),seasonal = c(0,1,1))
summary(sarima_mod)
checkresiduals(sarima_mod,lag.max=11)

sarima_pred <- forecast(sarima_mod, h=36)
autoplot(sarima_pred,include=156)
dat <- cbind(sarima_pred$mean,test)
matplot(dat, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)

library(Metrics)
rmse <- rmse(test$LandAverageTemperature, sarima_pred$mean)
smape <- smape(test$LandAverageTemperature, sarima_pred$mean)
global_metrics <- c(rmse, smape)
global_metrics

sarima_pred30 <- forecast(sarima_mod, h=216)
autoplot(sarima_pred30,include=156)
sarima_pred30$mean
write.zoo(sarima_pred30$mean, file = '~/downloads/pred30.csv', sep = ',')

pred30 <- read.csv("~/downloads/pred30.csv", header = FALSE)
typeof(pred30$V2)
length(pred30$V2)
library(Thermimage)
list30 <- .colMeans(pred30$V2, 12, 18)
length(list30)
list30

year <- (2013:2030)
df30 <- data.frame(Year = year, Average_Temperature = list30)
write.zoo(df30, file = '~/downloads/df30.csv', sep = ',')

#Major City

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

d_berlin <- stl(berlin_train, s.window="periodic")
plot(d_berlin)
d_cairo <- stl(cairo_train, s.window="periodic")
plot(d_cairo)
d_la <- stl(la_train, s.window="periodic")
plot(d_la)
d_mel <- stl(mel_train, s.window="periodic")
plot(d_mel)
d_mexico <- stl(mexico_train, s.window="periodic")
plot(d_mexico)
d_seoul <- stl(seoul_train, s.window="periodic")
plot(d_seoul)
d_shanghai <- stl(shanghai_train, s.window="periodic")
plot(d_shanghai)
d_london <- stl(london_train, s.window="periodic")
plot(d_london)

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
sarima_seoul <- Arima(seoul_train,order=c(0,1,1),seasonal = c(2,1,0))
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

autoplot(berlin_pred,include=156)
autoplot(cairo_pred,include=156)
autoplot(la_pred,include=156)
autoplot(mel_pred,include=156)
autoplot(mexico_pred,include=156)
autoplot(seoul_pred,include=156)
autoplot(shanghai_pred,include=156)
autoplot(london_pred,include=156)

dat1 <- cbind(berlin_pred$mean,berlin_test)
dat2 <- cbind(cairo_pred$mean,cairo_test)
dat3 <- cbind(la_pred$mean,la_test)
dat4 <- cbind(mel_pred$mean,mel_test)
dat5 <- cbind(mexico_pred$mean,mexico_test)
dat6 <- cbind(seoul_pred$mean,seoul_test)
dat7 <- cbind(shanghai_pred$mean,shanghai_test)
dat8 <- cbind(london_pred$mean,london_test)

matplot(dat1, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat2, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat3, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat4, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat5, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat6, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat7, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)
matplot(dat8, type = c("b"),pch=1,col = 1:2,)
legend("topleft", legend = c('prediction','actual'), col=1:2, pch=1)


city <- c('Berlin', 'Cairo', 'Los Angeles', 'Melbourne', 'Mexico', 'Seoul', 'Shanghai', 'London')
rmse <- c(berlin_rmse, cairo_rmse, la_rmse, mel_rmse, mexico_rmse, seoul_rmse, shanghai_rmse, london_rmse)
smape <- c(berlin_smape, cairo_smape, la_smape, mel_smape, mexico_smape, seoul_smape, shanghai_smape, london_smape)

data.frame(City = city, RMSE = rmse, sMAPE = smape)

nan <- c(1.915373, 4.660167, 5.627005, 3.081471, 2.119575, 1.457867, 8.090076, 1.669380)
royce <- rmse
will <- c(2.032635, 1.301818, 1.311217, 0.890430, 0.930498, 2.139120, 1.387452, 1.593907)
jaki <- c(1.868114, 1.522044, 1.328693, 1.202162, 1.126964, 2.164168, 1.411737, 1.752233)
data.frame(City = city, ARIMA = nan, SARIMA = royce, LSTM = will, Gluonts = jaki)
