
# Load libraries
library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")

# load assessment results
load("data/assessmemt.RData")


# build up a data.frame for the forecast
age_data <- data.frame(Age = 0:30)


# get fleet definitions
defs <- data.frame(t(assessmt$definitions[, -1]))
names(defs) <- gsub("^([a-zA-Z_]+)[#(:].*$", "\\1", assessmt$definitions[, 1])
defs[] <- lapply(defs, type.convert, as.is = TRUE)


# weights at age in the recreational fleet
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$current_year &
      Fleet == defs$fleet_ID[defs$fleet_names == "RecFish"] &
      Factor == "bodywt"
  ) %>%
  select("0":"30") %>%
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
  select("0":"30") %>%
  pivot_longer(everything(), names_to = "Age", values_to = "weights_age") %>%
  mutate(
    weights_age = ifelse(Age == 0, 0, weights_age),
    Age = as.numeric(Age)
  ) %>%
  right_join(age_data, by = "Age")


# stock weights
age_data <-
  assessmt$endgrowth %>%
  select(
    Age,
    Wt_Beg
  ) %>%
  rename(stkwt = Wt_Beg) %>%
  right_join(age_data, by = "Age")



# population at age in current year

natage <-
  assessmt$natage %>%
  filter(Yr == 2020 & `Beg/Mid` == "B") %>%
  select("0":"30") %>%
  t() %>%
  c()

gm <- function(x) exp(mean(log(unlist(x))))
# 2020 age 0 replaced by 2008-2017 GM;
natage[1] <-
  assessmt$natage %>%
  filter(Yr %in% 2008:2017 & `Beg/Mid` == "B") %>%
  select("0") %>%
  gm()

# 2020 age 1 replaced by SS3 survivor estimate at age 1, 2020 * GM / SS3 estimate of age 0, 2019
natage[2] <- natage[2] * natage_0 / filter(assessmt$natage, Yr == 2019 & `Beg/Mid` == "B")[["0"]]
natage[3] <- natage[3] * natage_0 / filter(assessmt$natage, Yr == 2018 & `Beg/Mid` == "B")[["0"]]

age_data$N <- natage

## Natural mortality
age_data <-
  assessmt$M_at_age %>%
  filter(Year == 2019) %>%
  select("0":"30") %>%
  pivot_longer(everything(), names_to = "Age", values_to = "M") %>%
  mutate(
    M = ifelse(is.na(M), mean(M, na.rm = TRUE), M),
    Age = as.numeric(Age)
  ) %>%
  right_join(age_data, by = "Age")


# F at age
fatage <-
  assessmt$Z_at_age %>% filter(Year %in% 2017:2019) %>% select("0":"30") -
    assessmt$M_at_age %>% filter(Year %in% 2017:2019) %>% select("0":"30")

fatage <- unname(unlist(colMeans(fatage)))

# catch at age
catage <- t(assessmt$catage[assessmt$catage$Yr == 2019, paste(0:30)])

# F for recreational
age_data$F_age_rec_2019 <- catage[, 6] / rowSums(catage) * fatage
age_data$F_age_rec_2019 <- ifelse(is.nan(age_data$F_age_rec_2019), 0, age_data$F_age_rec_2019)

# F of recreational fishery in 2012
fatage <-
  assessmt$Z_at_age %>%
  filter(Year %in% 2012) %>%
  select("0":"30") -
  assessmt$M_at_age %>%
  filter(Year %in% 2012) %>%
  select("0":"30")
fatage <- unname(unlist(colMeans(fatage)))

# catch at age
catage <- t(assessmt$catage[assessmt$catage$Yr == 2012, paste(0:30)])

# F for recreational
age_data$F_age_rec_2012 <- catage[, 6] / rowSums(catage) * fatage
age_data$F_age_rec_2012 <- ifelse(is.nan(age_data$F_age_rec_2012), 0, age_data$F_age_rec_2012)






# process plus group
age_data$stkwt[age_data$Age == 16] <- sum((age_data$N * age_data$stkwt)[age_data$Age >= 16]) / sum(age_data$N[age_data$Age >= 16])
age_data$N[age_data$Age == 16] <- sum(age_data$N[age_data$Age >= 16])

age_data <- filter(age_data, Age <= 16)

# maturity
age_data$mat <- c(0, 0, 0, 0, 0.089, 0.291, 0.575, 0.798, 0.916, 0.966, 0.986, 0.994, 0.997, 0.999, 0.999, 1, 1)


# Use advice forecast discard F selectivity for all fleet (quantity of disards by fleet from discard proportion estimates, below)
age_data <-
  left_join(
    age_data,
    read.csv("bootstrap/data/other/discard_selectivity.csv"),
    by = "Age"
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
