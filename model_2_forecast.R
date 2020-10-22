library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

# Additional functions
source("utilities_setup_input.R")
source("utilities_shiny.R")
source("utilities_gearCatches.R")
source("utilities_forecast.R")

# Read in required data
selectivity_age <- read.taf("data/gear_selectivity_age.csv")
load("model/setup_react.RData")
load("data/other_data.RData")

# run forecast

gear_catches <- setup_react$data[-nrow(setup_react$data), ]
gear_catches[is.na(gear_catches)] <- 0
gear_catches <- calc_tonnes(gear_catches, setup_react$setup$noVessels)
forecast <- run_forecast(gear_catches, selectivity_age, setup_react$setup, other_data)

save(forecast, file = "model/forecast.RData")
