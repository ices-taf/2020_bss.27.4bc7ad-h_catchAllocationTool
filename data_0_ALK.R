
# Load libraries
library(r4ss)
library(readr)
library(ggplot2)
library(dplyr)


# Define assessment folder and extract outputs
assessmt <-
  SS_output(taf.data.path("assessment"), covar = FALSE, printstats = FALSE)

# Extract population ages
assessmt_age <- assessmt$natage[assessmt$natage$`Beg/Mid`=="B",]
# ignore warnings
pop_age <-
  assessmt_age[
    ,
    c("Yr", names(assessmt_age)[!is.na(as.numeric(names(assessmt_age)))])
  ]

# Extract population lengths
assessmt_len <- assessmt$natlen[assessmt$natlen$`Beg/Mid`=="B",]
# ignore warnings
pop_len <-
  assessmt_len[
    ,
    c("Yr", names(assessmt_len)[!is.na(as.numeric(names(assessmt_len)))])
  ]

pop_len_mm <- as.integer(unique(names(pop_len)[-1])) * 10


# Read RDB files on age at length samples
age <- read_csv(taf.data.path("RDB", "RDB seabass Age and length data_.csv"))
age[1:10,]

age <-
  age[
    ,
    c(
      "Year", "LandingCountry", "FlagCountry", "Month", "Metierlvl4",
      "LengthClassInMm", "Age", "NumberOfFish"
    )
  ]

# year
yr_idx <- c(2017,2018)

# subset for years
age <- age[age$Year %in% yr_idx,]

# Collapse the age length key by quarters

#define quarters
age$quarter <- NA
age$quarter[age$Month %in% c(1,2,3)] <- 1
age$quarter[age$Month %in% c(4,5,6)] <- 2
age$quarter[age$Month %in% c(7,8,9)] <- 3
age$quarter[age$Month %in% c(10,11,12)] <- 4

#bin observation length categories as in population i.e. 2 cm bins
age$LengthClass <- as.numeric(as.character(age$LengthClassInMm))
age$bins <-
  cut(
    age$LengthClass,
    breaks = pop_len_mm * 10,
    right = FALSE,
    labels = pop_len_mm[-length(pop_len_mm)]
  )
unique(cbind.data.frame(age$bins,age$LengthClass))

age$bins <- as.numeric(as.character(age$bins))
# make plus group the largest length group of the assessment population
age$bins[age$LengthClass > max(pop_len_mm) - 1] <- max(pop_len_mm)
age$LengthClass <- age$bins
age[1:10,]

# aggregate key
age <-
  aggregate(NumberOfFish ~ Year + quarter + LengthClass + Age, age, sum)
age[1:10,]

# calculate proportions ages for each length class
age <-
  age %>%
  group_by(Year, quarter, LengthClass) %>%
  mutate(total_measured = sum(NumberOfFish)) %>%
  ungroup() %>%
  mutate(age_prop = NumberOfFish / total_measured)

# to save
ALK <- age[,c("Year","quarter","LengthClass","Age","age_prop")]
ALK[1:10,]

# save table
write.taf(ALK, dir = "data")
