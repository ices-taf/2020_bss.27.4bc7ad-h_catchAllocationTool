
library(icesTAF)
library(icesAdvice)

mkdir("report")

source("utilities_forecast.R")
load("model/setup_react.RData")
load("data/other_data.RData")

forecast_summary <- summarise_forecast(forecast, setup_react$setup)


catchGearTable <- catchGearTable(forecast_summary)
write.csv(catchGearTable, file = "report/CatchGearTable.csv", row.names = FALSE)

vclsGearTable <- vclsGearTable(forecast_summary, setup_react$setup)
write.csv(vclsGearTable, file = "report/vclsGearTable.csv", row.names = FALSE)

forecastTable <- forecastTable(forecast_summary, setup_react$setup, other_data)
write.csv(forecastTable, file = "report/forecastTable.csv", row.names = FALSE)
