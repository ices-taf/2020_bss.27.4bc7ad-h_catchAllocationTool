library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

# Additional functions
source("./utilities_setup_input.R")
source("./utilities_shiny.R")
source("./utilities_gearCatches.R")

# Read in required data
load("data/other_data.RData")
load("model/input.RData")
selectivity_age <- read.taf("data/gear_selectivity_age.csv")
age_data <- input$age_data

# get input
load("model/input.RData")

# run forecast
# Forecast <-

# Gear types
gears <- names(selectivity_age)[-1]

# Population matrix (Jan 2020 to Jan 2021)
initPop <- matrix(NA, 17, input$TimeStep + 1, dimnames = list(0:16, NULL))
initPop[, 1] <- age_data$N

# the forecast optimisation ---------
# -----------------------------------

out <- list()
# Switch for whether there is quota left or not (starts TRUE, changes to FALSE when quota used up.
# Rest of the months then have zero comm catch)
quota_left <- TRUE
catches <- input$catchGear

# loop over time steps
for (i in 1:input$TimeStep) {

  if (input$TimeStep == 12) {
    # check how much of TAC has been taken
    if (quota_left) {
      caught <- 0
      if (i > 1) {
        for (ii in 1:(i - 1)) {
          caught <- caught + sum(out[[ii]]$gearCatches)
        }
      }
      remaining <- input$ICESadvComm - caught
      # if quota is exceeded, scale catches, and set remaining to zero
      if (sum(catches[i, ]) > remaining) {
        catches[i, ] <- catches[i, ] * (remaining / sum(catches[i, ]))
        if (i != 12) {
          for (ii in (i + 1):12) {
            catches[ii, ] <- 0
          }
        }
        quota_left <- FALSE
      }
    }
  }

  # Split catches in landings and discards
  discard_prop <- other_data$discard_prop[names(catches)]
  landings_and_discards <-
    c(
      unlist(catches[i, ] * (1 - discard_prop)),
      Discards = sum(catches[i, ] * discard_prop)
    )

  # optimise Fmults to take the catches specified
  opt <-
    optim(
      rep(0, length(landings_and_discards)),
      objective_func,
      gearcatch = landings_and_discards,
      age_data = age_data,
      selectivity_age = selectivity_age,
      TimeStep = input$TimeStep,
      lower = rep(0, length(landings_and_discards)),
      method = "L-BFGS-B"
    )

  # Use optimised fmults to get catch.n, commercial F and total Z
  forecast <- gearCatches(opt$par, age_data, selectivity_age, TimeStep = input$TimeStep, quick = FALSE)

  # Project population forward
  # Note, ages unchanged, for Jan2021 shifted one age older after this loop
  initPop[, i + 1] <- initPop[, i] * exp(-forecast$total_z)

  # Save monthly results in list
  out[[i]] <- forecast
}

#
save(out, catches, file = "model/out.RData")
