## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

source("data_0_ALK.R")
source("data_0_lenfreq.R")

source("data_1_landings-selectivities.R")
source("data_2_selectivity-curves.R")

# not got this yet
# source("data_3_shiny.R")
