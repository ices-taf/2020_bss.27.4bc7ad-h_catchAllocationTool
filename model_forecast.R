library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

# Additional functions
source("./utilities_setup_input.R")
source("./utilities_shiny.R")
source("./utilities_gearCatches.R")
source("./utilities_forecast.R")

# Read in required data
selectivity_age <- read.taf("data/gear_selectivity_age.csv")
load("model/input.RData")
load("data/other_data.RData")

# run forecast
forecast <-
  run_forecast(gear_catches = input$gearCatches, selectivity_age, input, other_data)

save(forecast, file = "model/forecast.RData")
