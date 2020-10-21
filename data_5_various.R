
# Load libraries
library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")


other_data <- list()

# Fbar of recreational fishery in 2012
other_data$Fbar_rec_2012 <- 0.0604



## ICES advice (http://ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/bss.27.4bc7ad-h.pdf)
# Options for MAP
other_data$ICESadvMSY <- 1946
other_data$ICESadvMSYlow <- 1634
# Recreational catches from catch scenario (not used as a limit, just for comparison)
other_data$ICESadvMSYRec <- 412
other_data$ICESadvMSYlowRec <- 346

## Discards
# discard proportions by gear (from last 3 years of French and English data)
# adjusted to match assumed total discard rate in the advice forecast
# no discard selectivites available, so projection simply uses gear landings selectivities
# discard proportion is used on the results to divide total catch by gear in landings and discards
# by gear (only C@A will be presented, no L@A or D@A)
other_data$discard_prop <- read.taf("bootstrap/data/other/discard_proportions.csv")

other_data$noVessels <- read.taf("bootstrap/data/other/Number_Vessels.csv")

## Recreational fisheries
# F multipliers for bag limits and closed seasons
other_data$RecFs <- read.taf("bootstrap/data/BagLimitFs.csv")

other_data$AdviceScenarios <- read.taf("bootstrap/data/other/bss.27.4bc7ad-h 2019 Advice scenarios.csv")

save(other_data, file = "data/other_data.RData")
