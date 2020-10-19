
# Load libraries
library(icesTAF)
library(r4ss)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")

# Define assessment folder and extract outputs
assessmt <-
  SS_output(taf.data.path("assessment"), covar = FALSE, printstats = FALSE)

# get fleet definitions
defs <- data.frame(t(assessmt$definitions[,-1]))
names(defs) <- gsub("^([a-zA-Z_]+)[#(:].*$", "\\1", assessmt$definitions[,1])
defs[] <- lapply(defs, type.convert, as.is = TRUE)
str(defs)


# weights at age in the recreational fleet
weights_age_Rec <-
  assessmt$ageselex %>%
    filter(
      Yr ==  globals$yr_idx[2] &
        Fleet == defs$fleet_ID[defs$fleet_names == "RecFish"] &
        Factor == "bodywt"
    ) %>%
    select("0":"16") %>%
    pivot_longer(everything(), names_to = "Age", values_to = "Weight") %>%
    mutate(Weight = ifelse(Age == 0, 0, Weight))

write.taf(weights_age_Rec, dir = "shiny/data")


# weights at age in other fleets
# used were "other" or "French" == "Com D"
weights_age <-
  assessmt$ageselex %>%
    filter(
      Yr == globals$yr_idx[2] &
        Fleet == defs$fleet_ID[defs$fleet_names == "French"] &
        Factor == "bodywt"
    ) %>%
    select("0":"16") %>%
    pivot_longer(everything(), names_to = "Age", values_to = "Weight") %>%
    mutate(Weight = ifelse(Age == 0, 0, Weight))

write.taf(weights_age, dir = "shiny/data")








# F at age
Fatage <- assessmt$Z_at_age - assessmt$M_at_age
row.names(Fatage) <- assessmt$Z_at_age$Year
Fatage <- Fatage[,-(1:3)]
Fatage[,ncol(Fatage)] <- Fatage[,ncol(Fatage) - 1]

# catage last year
catage <- assessmt$catage[c("Fleet", "Yr", paste(0:30))]
catage_lasyear <- filter(catage, Yr == globals$yr_idx[2])

discard <- assessmt$discard[c("Fleet", "Yr", paste(0:30))]
catage_lasyear <- filter(catage, Yr == globals$yr_idx[2])


# partial Fs
pFatage <-
  Fatage[paste(globals$yr_idx[2]),] *
    filter(catage_lasyear, Fleet == recFleet$fleet_ID)[-(1:2)] /
    colSums(catage_lasyear[-(1:2)])


# lots of files to generate here:
# * weights at age
# * discard selectivity
# * whatever we can get from the assessment object





# modelled selectivity
selectivity_age <- read.csv("data/selectivity_fits_age.csv")
selectivity_age <-
  selectivity_age %>%
  select(Age, Selectivity, gear) %>%
  filter(
    gear %in% c("Demtrawl", "Gill", "HookLine", "Seines")
  ) %>%
  arrange(gear, Age)
# rename gear names


write.taf(selectivity_age, dir = "shiny/data")
