
# Load libraries
library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")

# build up a data.frame for the forecast
age_data <- data.frame(Age = globals$age)

# load assessment results
load("data/assessmemt.RData")

# get fleet definitions
defs <- data.frame(t(assessmt$definitions[, -1]))
names(defs) <- gsub("^([a-zA-Z_]+)[#(:].*$", "\\1", assessmt$definitions[, 1])
defs[] <- lapply(defs, type.convert, as.is = TRUE)


# weights at age in the recreational fleet
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$yr_idx[2] &
      Fleet == defs$fleet_ID[defs$fleet_names == "RecFish"] &
      Factor == "bodywt"
  ) %>%
  select("0":"16") %>%
  pivot_longer(everything(), names_to = "Age", values_to = "Weight") %>%
  mutate(
    Weight = ifelse(Age == 0, 0, Weight),
    Age = as.numeric(Age)
  ) %>%
  rename(weights_age_rec = Weight) %>%
  right_join(age_data, by = "Age")


# weights at age in other fleets
# used were "other" or "French" == "Com D"
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$yr_idx[2] &
      Fleet == defs$fleet_ID[defs$fleet_names == "French"] &
      Factor == "bodywt"
  ) %>%
  select("0":"16") %>%
  pivot_longer(everything(), names_to = "Age", values_to = "Weight") %>%
  mutate(
    Weight = ifelse(Age == 0, 0, Weight),
    Age = as.numeric(Age)
  ) %>%
  rename(weights_age = Weight) %>%
  right_join(age_data, by = "Age")


# stock weights
age_data$stkwt <- c(
  0.00282457, 0.0237327, 0.0961958, 0.209295, 0.368655, 0.569804, 0.806228, 1.07064, 1.35577,
  1.65483, 1.96175, 2.27132, 2.57917, 2.88175, 3.17626, 3.46058, 3.73313
)





# population at age in current year
age_data <-
  left_join(
    age_data,
    read.csv("bootstrap/data/other/pop_age_2020.csv"),
    by = "Age"
  )


# F_age_rec_2019


# Use advice forecast discard F selectivity for all fleet (quantity of disards by fleet from discard proportion estimates, below)
age_data <-
  left_join(
    age_data,
    read.csv("bootstrap/data/other/discard_selectivity.csv"),
    by = "Age"
  )


# maturity
age_data$mat <- c(0, 0, 0, 0, 0.089, 0.291, 0.575, 0.798, 0.916, 0.966, 0.986, 0.994, 0.997, 0.999, 0.999, 1, 1)

## Natural mortality
# per year
age_data$Myr <- 0.24
# per Month
age_data$M <- age_data$Myr / 12

# F for recreational
age_data$F_age_rec_2019 <- c(
  0, 0.000205609, 0.00118539, 0.001176412, 0.006351395, 0.013477775, 0.012815346,
  0.023498693, 0.014668892, 0.022002183, 0.020376389, 0.023425403, 0.023519036,
  0.021145238, 0.021742123, 0.022936007, 0.024790632
)


# Z at age from the ICES advice forecasts
# This is used to estimate total Recreational catch
age_data <-
  left_join(
    age_data,
    read.csv("bootstrap/data/other/ICESadvice_Forecast_Zs.csv"),
    by = "Age"
  )

# Forecasted Catch at age
age_data <-
  left_join(
    age_data,
    read.csv("bootstrap/data/other/AdviceForecastCatchAge.csv"),
    by = "Age"
  )

write.taf(age_data, dir = "data")



# attempt to get this from assessment obj

# F at age
Fatage <- assessmt$Z_at_age - assessmt$M_at_age
row.names(Fatage) <- assessmt$Z_at_age$Year
Fatage <- Fatage[, -(1:3)]
Fatage[, ncol(Fatage)] <- Fatage[, ncol(Fatage) - 1]

# catage last year
catage <- assessmt$catage[c("Fleet", "Yr", paste(0:30))]
catage_lasyear <- filter(catage, Yr == globals$yr_idx[2])

# discard <- assessmt$discard[c("Fleet", "Yr", paste(0:30))]
# catage_lasyear <- filter(catage, Yr == globals$yr_idx[2])

# partial Fs
pFatage <-
  Fatage[paste(globals$yr_idx[2]), ] *
    filter(catage_lasyear, Fleet == defs$fleet_ID[defs$fleet_names == "RecFish"])[-(1:2)] /
    colSums(catage_lasyear[-(1:2)])
