## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

sourceTAF("data_0_assessment.R")

sourceTAF("data_1_ALK.R")
sourceTAF("data_2_lenfreq.R")

sourceTAF("data_3_landings-selectivities.R")
sourceTAF("data_4_selectivity-curves.R")

# not got this yet
# source("data_5_shiny.R")
