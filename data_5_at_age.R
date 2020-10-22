
# Load libraries
library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")

# year
load("data/globals.RData")

# load assessment results
load("data/assessmemt.RData")


# overwrite modelled selectivity -
selectivity_age_old <-
  read.taf(taf.data.path("other", "selectivity_age_16+.csv")) %>%
  mutate(
    gear = gsub("_", " ", gear)
  ) %>%
  pivot_wider(names_from = gear, values_from = Selectivity)

#selectivity_age_new <- read.taf("data/gear_selectivity_age.csv")
# selectivity_age_old - selectivity_age_new

write.taf(selectivity_age_old, file = "data/gear_selectivity_age.csv")




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


# catch at age
#catage <- t(assessmt$catage[assessmt$catage$Yr == 2018, paste(0:30)])








# weights at age in other fleets
# used were "other" or "French" == "Com D"
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$current_year &
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
age_data$F_age_rec_2020 <- catage[, 6] / rowSums(catage) * fatage
age_data$F_age_rec_2020 <- ifelse(is.nan(age_data$F_age_rec_2020), 0, age_data$F_age_rec_2020)


# F at age in discards
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$current_year &
      Fleet %in% defs$fleet_ID[defs$fleet_names %in% c("UKOTB_Nets", "French")] &
      Factor %in% c("sel*ret_nums", "dead_nums")
  ) %>%
  select(
    Factor, Fleet, "0":"30"
  ) %>%
  pivot_longer(
    "0":"30",
    names_to = "Age", values_to = "sel"
  ) %>%
    pivot_wider(
      names_from = Factor,
      values_from = sel
    ) %>%
  mutate(
    Age = as.numeric(Age),
    ret_frac = ifelse(dead_nums == 0, 0, `sel*ret_nums` / dead_nums)
  ) %>%
  select(Fleet, Age, ret_frac) %>%
  mutate(
    Ffleet = c(catage[, 1], catage[, 4]) / rowSums(catage) * fatage
  ) %>%
  group_by(Age) %>%
  summarise(
    F_age_disc_2020 = sum(Ffleet * (1 - ret_frac))
  ) %>%
  mutate(
    F_age_disc_2020 = ifelse(is.nan(F_age_disc_2020), 0, F_age_disc_2020),
    Discard_Sel = F_age_disc_2020 / sum(F_age_disc_2020, na.rm = TRUE)
  ) %>%
  right_join(age_data, by = "Age")

# F at age in landinds
age_data <-
  assessmt$ageselex %>%
  filter(
    Yr == globals$current_year &
      Fleet %in% defs$fleet_ID[defs$fleet_names %in% c("UKOTB_Nets", "Lines", "UKMWT", "French", "Other")] &
      Factor %in% c("sel*ret_nums", "dead_nums")
  ) %>%
  select(
    Factor, Fleet, "0":"30"
  ) %>%
  pivot_longer(
    "0":"30",
    names_to = "Age", values_to = "sel"
  ) %>%
  pivot_wider(
    names_from = Factor,
    values_from = sel
  ) %>%
  mutate(
    Age = as.numeric(Age),
    ret_frac = ifelse(dead_nums == 0, 0, `sel*ret_nums` / dead_nums)
  ) %>%
  select(Fleet, Age, ret_frac) %>%
  mutate(
    ret_frac = ifelse(Fleet %in% c(1, 4), ret_frac, 1),
    Ffleet = c(catage[, 1], catage[, 2], catage[, 3], catage[, 4], catage[, 5]) / rowSums(catage) * fatage
  ) %>%
  group_by(Age) %>%
  summarise(
    F_age_land_2020 = sum(Ffleet * ret_frac)
  ) %>%
  mutate(
    F_age_land_2020 = ifelse(is.nan(F_age_land_2020), 0, F_age_land_2020)
  ) %>%
  right_join(age_data, by = "Age")

recF_multiplier <- 1.428

age_data <-
  age_data %>%
  mutate(
    Z_age_2020 = M + F_age_land_2020 + F_age_disc_2020 + F_age_rec_2020 * recF_multiplier
  )



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
natage[2] <- natage[2] * natage[1] / filter(assessmt$natage, Yr == 2019 & `Beg/Mid` == "B")[["0"]]
natage[3] <- natage[3] * natage[1] / filter(assessmt$natage, Yr == 2018 & `Beg/Mid` == "B")[["0"]]

# roll forward to 2021!
age_data$N_2020 <- natage
age_data$N <- c(natage[1], natage * exp(-age_data$Z_age_2020))[-(nrow(age_data) + 1)]

# maturity
age_data$mat <-
  c(
    0.000, 0.000, 0.000, 0.000, 0.095, 0.300, 0.580, 0.797, 0.914, 0.964, 0.985, 0.993, 0.997,
    0.998, 0.999, 0.999, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
    1.000, 1.000, 1.000, 1.000, 1.000
  )


# process plus group
age_data$stkwt[age_data$Age == 16] <- sum((age_data$N_2020 * age_data$stkwt)[age_data$Age >= 16]) / sum(age_data$N_2020[age_data$Age >= 16])
age_data$N[age_data$Age == 16] <- sum(age_data$N[age_data$Age >= 16])
age_data$N_2020[age_data$Age == 16] <- sum(age_data$N_2020[age_data$Age >= 16])

age_data <- filter(age_data, Age <= 16)


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

# overwrite till its fixed
warning("overwriting landings weights at age")
age_data$weights_age <- read.taf(taf.data.path("checks", "forecast_inputs.csv"))$weights_age

write.taf(age_data, dir = "data")
