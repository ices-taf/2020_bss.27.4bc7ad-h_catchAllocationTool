
# Load libraries
library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")

age_data <- read.taf("data/age_data.csv")
advice <- read.taf(taf.data.path("other", "bss.27.4bc7ad-h 2019 Advice scenarios.csv"))


other_data <- list()

# for the % change in catch options
## ssb at start of advice year
other_data$ssb_ref <- 11451
# total catch
other_data$advice_ref <- list(MSYlow = 1634, MSY = 1946)

## Discards
# discard proportions by gear (from last 3 years of French and English data)
# adjusted to match assumed total discard rate in the advice forecast
# no discard selectivites available, so projection simply uses gear landings selectivities
# discard proportion is used on the results to divide total catch by gear in landings and discards
# by gear (only C@A will be presented, no L@A or D@A)
other_data$discard_prop <- read.taf("bootstrap/data/other/discard_proportions.csv")


other_data$noVessels <- read.taf("bootstrap/data/Number_Vessels.csv")

## Recreational fisheries
# F multipliers for bag limits and closed seasons
other_data$RecFs <- read.taf("bootstrap/data/BagLimitFs.csv")


other_data$AdviceScenarios <- advice


## ICES advice (http://ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/bss.27.4bc7ad-h.pdf)
# Options for MAP
other_data$ICESadvMSY <- advice[advice$Basis == "EU MAP FMSY", "Total catch (2021)"]
other_data$ICESadvMSYlow <- advice[advice$Basis == "EU MAP FMSY lower", "Total catch (2021)"]
# Recreational catches from catch scenario (not used as a limit, just for comparison)
other_data$ICESadvMSYRec <- advice[advice$Basis == "EU MAP FMSY", "Recreational removals (2021)"]
other_data$ICESadvMSYlowRec <- advice[advice$Basis == "EU MAP FMSY lower", "Recreational removals (2021)"]


# Fbar of recreational fishery in 2012
other_data$Fbar_rec_2012 <- mean(age_data$F_age_rec_2012[5:16])


save(other_data, file = "data/other_data.RData")
