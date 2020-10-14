## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

source("data_0_ALK.R")

source("data_1_landings-selectivities_ages.R")
source("data_2_selectivity-curves_age.R")
