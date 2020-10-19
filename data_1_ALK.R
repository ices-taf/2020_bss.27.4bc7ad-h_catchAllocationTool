
# Load libraries
library(r4ss)
library(readr)
library(ggplot2)
library(dplyr)

mkdir("data")

# year
load("data/globals.RData")

# load assessment results
load("data/assessmemt.RData")

# Extract population ages
pop_age <- as.integer(grep("[0-9]+", names(assessmt$natage), value = TRUE))

# Extract population lengths
pop_len_mm <-
  as.integer(
    grep("[0-9]+", names(assessmt$natlen), value = TRUE)
  ) * 10


# Read RDB files on age at length samples
age <- read_csv(
  taf.data.path("RDB", "RDB seabass Age and length data_.csv"),
  na = c("", "NA", "NULL"),
  col_types = cols(Sex = col_character())
)

# subset for years
# Collapse the age length key by quarters
#bin observation length categories as in population i.e. 2 cm bins
# and aggregate
age <-
  age %>%
  select(
    Year, LandingCountry, FlagCountry, Month, Metierlvl4,
      LengthClassInMm, Age, NumberOfFish
  ) %>%
  filter(
    Year %in% globals$yr_idx
  ) %>%
  mutate(
    LengthClass = pop_len_mm[
        findInterval(LengthClassInMm, pop_len_mm, rightmost.closed = TRUE)
      ],
    quarter = ceiling(Month * 4 / 12)
  ) %>%
  group_by(
    Year, quarter, LengthClass, Age
  ) %>%
  summarise(
    NumberOfFish = sum(NumberOfFish),
    .groups = "drop"
  )

# calculate proportions ages for each length class
ALK <-
  age %>%
  group_by(Year, quarter, LengthClass) %>%
  mutate(total_measured = sum(NumberOfFish)) %>%
  ungroup() %>%
  mutate(age_prop = NumberOfFish / total_measured) %>%
  select(
    Year, quarter, LengthClass, Age, age_prop
  )

# save table
write.taf(ALK, dir = "data")
