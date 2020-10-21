
# Load libraries
library(icesTAF)
library(r4ss)
library(readr)
library(ggplot2)
library(dplyr)

# year
load("data/globals.RData")

# load assessment results
load("data/assessmemt.RData")

# Read RDB files on length samples
len <- read_csv(taf.data.path("RDB", "RDB seabass length data_.csv"), col_types = cols())

# Extract population lengths
pop_len_mm <-
  as.integer(
    grep("[0-9]+", names(assessmt$natlen), value = TRUE)
  ) * 10

# Bin observation length categories as in population i.e. 2 cm bins
# and aggregate in the new bins,
# keep only landings and years of interest
len <-
  len %>%
  filter(
    CatchCategory == "LAN" &
      Year %in% globals$yr_idx
  ) %>%
  select(
    -SUM_NoAtLengthInSample, -CatchCategory, -LengthCode, -Species, -EnglishName, -Stock
  ) %>%
  mutate(
    LengthClass =
      pop_len_mm[
        findInterval(LengthClass, pop_len_mm, rightmost.closed = TRUE)
      ]
  ) %>%
  group_by(across(-SUM_NoAtLengthInCatch)) %>%
  summarise(
    SUM_NoAtLengthInCatch = sum(SUM_NoAtLengthInCatch),
    .groups = "drop"
  ) %>%
  group_by(across(c(-LengthClass, -SUM_NoAtLengthInCatch))) %>%
  mutate(
    tot = sum(SUM_NoAtLengthInCatch)
  ) %>%
  ungroup() %>%
  mutate(
    prop = SUM_NoAtLengthInCatch / tot
  )

# Read RDB files on landings samples
land <- read_csv(taf.data.path("RDB", "RDB seabass Landings data_.csv"), col_types = cols())

# keep only relevant flag countries (for which length data exist)
# and years
land <-
  land %>%
  filter(
    FlagCountry %in% unique(len$FlagCountry) &
      Year %in% globals$yr_idx,
  ) %>%
  select(
    -Stock, -Species, -SpeciesDesc
  )


gear_lookup <- read.taf(taf.data.path("gear_lookup", "gear_lookup.csv"))

# Merge length and landings and add quarter
# we are ignoring Landing Category (some IND, and some BMS) after merge
lenland <-
  full_join(
    len,
    land,
    by = c("FlagCountry", "LandingCountry", "Year", "Month", "Metierlvl4", "LandingCategory")
  ) %>%
  select(
    -LandingCategory
  ) %>%
  group_by(across(-SUM_NoAtLengthInCatch)) %>%
  summarise(
    SUM_NoAtLengthInCatch = sum(SUM_NoAtLengthInCatch),
    .groups = "drop"
  ) %>%
  mutate(
    quarter = ceiling(Month * 4 / 12)
  ) %>%
  left_join(gear_lookup, by = "Metierlvl4")

# For each line (i.e. year/month/countries/metier) with no length data fill in as follows:
# 1 - use the average from the quarter (remove month)
# 2 - use the average from the gear/quarter (remove month, metier)
# 3 - use the average from landcountry/gear/quarter (remove month, metier, flagcountry)
# 4 - use the average from year/landcountry/gear (remove month, metier, flagcountry,quarter)
# 5 - use the average for all countries /year/gear (remove month, metier, flagcountry,quarter, landcountry)
by_list <- list(
  "1" = c("FlagCountry", "LandingCountry", "Year", "Metierlvl4", "quarter", "gear"),
  "2" = c("FlagCountry", "LandingCountry", "Year", "quarter", "gear"),
  "3" = c("LandingCountry", "Year", "quarter", "gear"),
  "4" = c("LandingCountry", "Year", "gear"),
  "5" = c("Year", "gear")
)

lenland_noNA <- lenland[!is.na(lenland$prop), ]
lenland_NA <- filter(lenland, is.na(prop))

out <- NULL
warns <- integer(0)

for (i in 1:nrow(lenland_NA)) {
  sub <-
    lenland_NA[i, ] %>%
    select(-prop, -tot, -LengthClass, -SUM_NoAtLengthInCatch)
  for (by_arg in by_list) {
    len_freq <-
      sub %>%
      left_join(
        lenland_noNA %>%
          select(
            all_of(by_arg), LengthClass, SUM_NoAtLengthInCatch
          ),
        by = by_arg
      )

    if (!is.na(len_freq$LengthClass[1])) {
      break
    }
  }

  # cycle if
  if (is.na(len_freq$LengthClass[1])) {
    warns <- c(warns, i)
    next
  }

  len_freq <-
    len_freq %>%
    group_by(LengthClass) %>%
    summarise(
      SUM_NoAtLengthInCatch = sum(SUM_NoAtLengthInCatch),
      .groups = "drop"
    ) %>%
    mutate(
      tot = sum(SUM_NoAtLengthInCatch),
      prop = SUM_NoAtLengthInCatch / tot
    ) %>%
    cbind(
      lenland_NA[i, ] %>% select(-LengthClass, -SUM_NoAtLengthInCatch, -tot, -prop)
    ) %>%
    select(
      names(lenland)
    )

  out <- rbind(out, len_freq)
}

if (length(warns)) {
  txt <- paste(
    capture.output(print.data.frame(lenland_NA[warns, ])),
    collapse = "\n"
  )
  warning("NAs remain - check why:\n", txt)
  lenland_NA[warns, ]
}

# Combine filled in dataset with existing
lenland <- rbind(lenland[!is.na(lenland$prop), ], out)

# Apply the length frequencies to the catches
# so prop is n per kg??
lenland <- mutate(lenland, natlen = prop*SumOfficialLandingCatchWeightInKg)

# "lenland" is the length frequency at the most disaggregated level with gaps filled in
# Now summarise for length frequencies by quarter and gear type exclusively
lenfreqs <-
  lenland %>%
  group_by(
    Year, quarter, gear, LengthClass
  ) %>%
  summarise(
    natlen = sum(natlen),
    .groups = "drop"
  ) %>%
  group_by(
    Year, quarter, gear
  ) %>%
  mutate(
    tot = sum(natlen)
  )

write.taf(lenfreqs, dir = "data")
