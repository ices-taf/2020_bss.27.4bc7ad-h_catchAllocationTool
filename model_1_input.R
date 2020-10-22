library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

mkdir("model")

# Additional functions
source("utilities_setup_input.R")
source("utilities_shiny.R")

# Read in required data
age_data <- read.taf("data/age_data.csv")
load("data/other_data.RData")

# set up (useing defaults)
setup_react <-
  list(
    setup =
      setup_input(
        TimeStep = "1", AdviceType = "MSY",
        Comm_v_Rec = "1", OpenSeason = "2", BagLimit = "2",
        age_data = age_data, source_data = other_data
      )
  )

# enter some values for catches in tones by vessel
setup_react$data <- setup_react$setup$data
setup_react$data[1] <- 1000
setup_react$data <- calc_total(setup_react$data)

save(setup_react, file = "model/setup_react.RData")
