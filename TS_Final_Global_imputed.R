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
write.zoo(newglobal_xts, file = '~/downloads/imputed_Global.csv', sep = ',')
