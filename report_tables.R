
mkdir("report")
source("utilities_forecast.R")

forecast_summary <- summarise_forecast(forecast, input)


CatchGearTable <- catchGearTable(forecast_summary)
write.csv(CatchGearTable, file = "report/CatchGearTable.csv", row.names = FALSE)

VclsGearTable <- vclsGearTable(forecast_summary)
write.csv(VclsGearTable, file = "report/vclsGearTable.csv", row.names = FALSE)

forecastTable <- forecastTable(forecast_summary, input, other_data)
write.csv(forecastTable, file = "report/forecastTable.csv", row.names = FALSE)
