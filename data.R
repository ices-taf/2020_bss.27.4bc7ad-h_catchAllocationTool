## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# create global or configuration settings
sourceTAF("data_0_globals.R")

sourceTAF("data_0_assessment.R")


# a bug somewhere in here - using last years output instead
#sourceTAF("data_1_ALK.R")
#sourceTAF("data_2_lenfreq.R")
#sourceTAF("data_3_landings-selectivities.R")
#sourceTAF("data_4_selectivity-curves.R")

# create output for forecasts
sourceTAF("data_5_at_age.R")
sourceTAF("data_5_various.R")

sourceTAF("data_6_checks.R")
