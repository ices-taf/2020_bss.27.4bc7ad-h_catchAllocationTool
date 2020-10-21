
mkdir("report")
source("utilities_forecast.R")

forecast_summary <- summarise_forecast(forecast, input)


CatchGearTable <- catchGearTable(forecast_summary)
write.csv(CatchGearTable, file = "report/CatchGearTable.csv", row.names = FALSE)





## Catch gear table as VCLS
vclsGearTable <-
  cbind(
    Month = row.names(realisedCatch),
    round(calc_tonnes_by_vessel(realisedCatch[,gears], input$noVessels), 2)
  )
vclsGearTable["TOTAL", 1] <- "Annual catch/vessel"

write.csv(vclsGearTable, file = "report/vclsGearTable.csv", row.names = FALSE)




## Forecast table outputs
forecastTable <- matrix(NA_character_, ncol = 12, nrow = 1, dimnames = list(
  input$AdviceType,
  c(
    "Basis", "Total Catch", "Commercial Landings", "Commercial discards", "Recreational removals", "Total F", "F Commercial landings",
    "F Commercial discards", "F Recreational removals", "SSB (2021)", "% SSB change", "% Advice change"
  )
))

forecastTable[, "Basis"] <- "Simulated Scenario"
forecastTable[, "Total Catch"] <- round(totCommCatch + input$recCatch, 0)
forecastTable[, "Commercial Landings"] <- round(totCommLandings, 0)
forecastTable[, "Commercial discards"] <- round(totCommDiscards, 0)
forecastTable[, "Recreational removals"] <- round(input$recCatch, 0)
forecastTable[, "Total F"] <- Ftotbar
forecastTable[, "F Commercial landings"] <- Flandbar
forecastTable[, "F Commercial discards"] <- Fdisbar
forecastTable[, "F Recreational removals"] <- FbarRec
forecastTable[, "SSB (2021)"] <- round(ssb2021, 0)
forecastTable[, "% SSB change"] <- round(100 * (ssb2021 - 11413) / 11413, 1) # DM: change from fixed value
forecastTable[, "% Advice change"] <- round(100 * ((totCommCatch + input$recCatch) - 1806) / 1806, 1) # DM: change from fixed value

if (FALSE) {
  # some kind of fixed value here
  if (ICESadvOpt == "MSY") {
    forecastTable[, "% Advice change"] <- 7.8
  } else {
    forecastTable[, "% Advice change"] <- -9.5
  }
}

# 2019 Advice sheet catch scenarios
AdviceScenarios <- other_data$AdviceScenarios
names(AdviceScenarios) <- colnames(forecastTable)

forecastTable <- rbind(forecastTable, AdviceScenarios)

write.csv(forecastTable, file = "report/forecastTable.csv", row.names = FALSE)
