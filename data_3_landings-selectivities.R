
# Load libraries
library(icesTAF)
library(r4ss)
library(readr)
library(ggplot2)
library(dplyr)

# load assessment results
load("data/assessmemt.RData")
load("data/globals.RData")

# combine len freqs with ALK and aggregate
alk <- read_csv("data/ALK.csv", col_types = cols())
lenfreqs <- read_csv("data/lenfreqs.csv", col_types = cols())

agefreqs <-
  left_join(
    lenfreqs, alk, by = c("Year", "quarter", "LengthClass")
  ) %>%
  mutate(
    natage = natlen * age_prop,
    prop = natage / tot
  ) %>%
  group_by(
    Year, gear, Age
  ) %>%
  summarise(
    numbers = sum(natage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year, gear) %>%
  mutate(
    totals = sum(numbers, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(prop = numbers / totals)

# Plot population ages vs gear age frequencies
# Extract population ages
pop_age <-
  assessmt$natage[
    assessmt$natage$`Beg/Mid`=="B",
    c("Yr", grep("[0-9]+", names(assessmt$natage), value = TRUE))
  ]

#extract length vector for year yr_idx from the assessment
vecpop <- pop_age[pop_age$Yr %in% globals$yr_idx, -1]
vecpop <- as.vector(matrix(apply(vecpop, 1, function(x) x/sum(x)), nrow=1))
popdf <-
  cbind.data.frame(
    Year = rep(globals$yr_idx, each = length(as.numeric(names(pop_age)[-1]))),
    Age = rep(as.numeric(names(pop_age)[-1]), times = length(globals$yr_idx)),
    freq = vecpop
  )

# merge full sample dataset (including zeros) with population data - involves creating zeros first for unsampled length categories
Year <-
  rep(
    unique(agefreqs$Year),
    each = length(unique(agefreqs$gear)),
    times = length(unique(popdf$Age))
  )
gear <-
  rep(
    unique(agefreqs$gear),
    times = length(unique(agefreqs$Year)),
    each = length(unique(popdf$Age))
  )
Age <-
  rep(
    unique(popdf$Age),
    times = length(unique(agefreqs$gear)) * length(unique(agefreqs$Year))
  )
full_frame <- cbind.data.frame(Year,gear,Age)
agefreqs <- merge(full_frame, agefreqs, all.x = TRUE)
agefreqs$prop[is.na(agefreqs$prop)] <- 0
agefreqs <- merge(agefreqs, popdf, all = TRUE)

# Define selectivity
# using a nominal value of 10000 for total population as actual number in pop or catch just scalers
agefreqs$selex <- agefreqs$prop / (agefreqs$freq * 10000)
# save data for tool
agefreqs_save <- agefreqs
agefreqs_save$numbers <- NULL
agefreqs_save$totals <- NULL
names(agefreqs_save) <-
  c("yr", "Age", "gear_group", "freq_in_gear", "freq_in_pop", "selectivity")
ages_for_selectivity <- agefreqs_save

write.taf(ages_for_selectivity, dir = "data")


# Plot selectivity
agefreqs$Year <- as.factor(agefreqs$Year)
ggplot(agefreqs, aes(x = Age, y = selex, col = Year)) +
  geom_line() +
  facet_wrap(~gear)
# remove weird peak
ggplot(
  agefreqs[agefreqs$Age < 20, ],
  aes(x = Age, y = selex, col = Year)
) +
  geom_line() +
  facet_wrap(~gear)

ggplot(agefreqs[agefreqs$selex < 0.03, ], aes(x = Age, y = selex, col = Year)) +
  geom_line() +
  facet_wrap(~gear)
