library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

mkdir("model")

# Additional functions
source("./utilities_setup_input.R")
source("./utilities_shiny.R")

# Read in required data
age_data <- read.taf("data/age_data.csv")
load("data/other_data.RData")

# set up (useing defaults)
input <-
  setup_input(
    TimeStep = "12", AdviceType = "MSY",
    Comm_v_Rec = "1", OpenSeason = "2", BagLimit = "2",
    source_data = other_data
  )

# enter some values for catches in tones by vessel
input$data[] <- 0.05

# and calculate total tonnes per year
input$catchGear <- calc_tonnes(input$data, input$noVessels)

# end of inputs

save(input, file = "model/input.RData")
