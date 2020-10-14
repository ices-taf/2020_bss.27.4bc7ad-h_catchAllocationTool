
# Load libraries
library(r4ss)
library(readr)
library(ggplot2)

# Define assessment folder and extract outputs
assessmt <-
  SS_output(taf.data.path("assessment"), covar = FALSE, printstats = FALSE)

# Read RDB files on landings and length samples
land <- read_csv(taf.data.path("RDB", "RDB seabass Landings data_.csv"))
len <- read_csv(taf.data.path("RDB", "RDB seabass length data_.csv"))

# Select year of interest
yr_idx <- c(2017, 2018)

# Extract population lengths
assessmt_len <- assessmt$natlen[assessmt$natlen$`Beg/Mid`=="B",]
#ignore warnings
pop <- assessmt_len[,c("Yr",names(assessmt_len)[!is.na(as.numeric(names(assessmt_len)))])]

pop_len_mm <- as.integer(unique(names(pop)[-1])) * 10

# Group metiers of interest
land$gear <- land$Metierlvl4
land$gear[land$Metierlvl4 %in% c("GN","GNS","GTR","GND","FYK")] <- "Gill"
land$gear[land$Metierlvl4 %in% c("LHM","LHP","LLS","LLD","LTL","LX")] <- "HookLine"
land$gear[land$Metierlvl4 %in% c("PTB","PTM")] <- "PelTrawl"
land$gear[land$Metierlvl4 %in% c("TBB","OTB","OTM","OTT")] <- "Demtrawl"
land$gear[land$Metierlvl4 %in% c("SSC","SDN","SB")] <- "Seines"
land$gear[land$Metierlvl4 %in% c("PS")] <- "PurseSeine"
land$gear[land$Metierlvl4 %in% c("MIS","DRB","FPO","LA","HMD","No")] <- "Other"

# Change landings table variables to match lengths variables
land$LandingCountry[land$LandingCountry=="ENG"] <- "England"
land$LandingCountry[land$LandingCountry=="WLS"] <- "Wales"
land$LandingCountry[land$LandingCountry=="FRA"] <- "France"
land$LandingCountry[land$LandingCountry=="NLD"] <- "Netherlands"
land$LandingCountry[land$LandingCountry=="NIR"] <- "Northern Ireland"
land$LandingCountry[land$LandingCountry=="SCT"] <- "Scotland"
land$FlagCountry[land$FlagCountry=="ENG"] <- "England"
land$FlagCountry[land$FlagCountry=="WLS"] <- "Wales"
land$FlagCountry[land$FlagCountry=="FRA"] <- "France"
land$FlagCountry[land$FlagCountry=="NLD"] <- "Netherlands"
land$FlagCountry[land$FlagCountry=="NIR"] <- "Northern Ireland"
land$FlagCountry[land$FlagCountry=="SCT"] <- "Scotland"

# Clean up and keep only relevant flag countries (for which length data exist)
land2 <- land[land$FlagCountry %in% c("England","Wales","France","Netherlands",
                                      "Northern Ireland","Scotland"),]

# Bin observation length categories as in population i.e. 2 cm bins
len$LengthClass <- as.numeric(as.character(len$LengthClass))
len$bins <-
  cut(
    len$LengthClass,
    breaks = pop_len_mm, right = FALSE,
    labels = pop_len_mm[-length(pop_len_mm)]
  )
unique(cbind.data.frame(len$bins,len$LengthClass))
len$bins <- as.numeric(as.character(len$bins))
# make plus group the largest length group of the assessment population
len$bins[len$LengthClass > max(pop_len_mm) - 1] <- max(pop_len_mm)
len$LengthClass <- len$bins
# aggregate in the new bins
len <-
  aggregate(
    SUM_NoAtLengthInCatch ~ LandingCountry + FlagCountry + Year + Month +
      Stock + Species + EnglishName + Metierlvl4 + CatchCategory +
      LandingCategory + LengthClass,
    len, sum
  )

# Merge length and landings
lenlandAll <-
  merge(
    land2[land2$Year %in% yr_idx, ],
    len[len$CatchCategory == "LAN" & len$Year %in% yr_idx, ],
    all.x = TRUE
  )

# Get length frequency by gear type
lenlandAllAgg <-
  aggregate(
    SUM_NoAtLengthInCatch ~ LandingCountry + FlagCountry + Year + Month +
      Stock + Species + EnglishName + gear + Metierlvl4 + CatchCategory +
      LandingCategory + SumOfficialLandingCatchWeightInKg + LengthClass,
    lenlandAll, sum
  )
lenlandAllAggSum <-
  aggregate(
    SUM_NoAtLengthInCatch ~ LandingCountry + FlagCountry + Year + Month +
      Stock + Species + EnglishName + gear + Metierlvl4 + CatchCategory +
      LandingCategory + SumOfficialLandingCatchWeightInKg, #+ LengthClass,
    lenlandAllAgg, sum
  )
names(lenlandAllAggSum)[ncol(lenlandAllAggSum)] <- "tot"
lenlandAll2 <- merge(lenlandAllAgg, lenlandAllAggSum)
#frequency
lenlandAll2$prop <- lenlandAll2$SUM_NoAtLengthInCatch / lenlandAll2$tot

# Fill in gaps in data
# first merge with full combined landings and lengths to keep NAs
#  (they were remove during the aggregations process above)
len_raise <- merge(lenlandAll, lenlandAll2, all.x = TRUE)

# Define quarters
len_raise$quarter[len_raise$Month %in% c(1,2,3)] <- 1
len_raise$quarter[len_raise$Month %in% c(4,5,6)] <- 2
len_raise$quarter[len_raise$Month %in% c(7,8,9)] <- 3
len_raise$quarter[len_raise$Month %in% c(10,11,12)] <- 4

# For each line (i.e. year/month/countries/metier) with no length data fill in as follows:
# 1 - use the average from the quarter (remove month)
# 2 - use the average from the gear/quarter (remove month, metier)
# 3 - use the average from landcountry/gear/quarter (remove month, metier, flagcountry)
# 4 - use the average from year/landcountry/gear (remove month, metier, flagcountry,quarter)
# 5 - use the average for all countries /year/gear (remove month, metier, flagcountry,quarter, landcountry)
len_raise_NA   <- len_raise[is.na(len_raise$prop),]
len_raise_noNA <- len_raise[!is.na(len_raise$prop),]
out <- NULL
for (i in 1:nrow(len_raise_NA)) {
  sub <- len_raise_NA[i,]
  if (is.na(sub$prop)) {
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$LandingCountry == sub$LandingCountry &
          len_raise_noNA$FlagCountry == sub$FlagCountry &
          len_raise_noNA$Metierlvl4 == sub$Metierlvl4 &
          len_raise_noNA$quarter == sub$quarter,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])) {
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$LandingCountry == sub$LandingCountry &
          len_raise_noNA$FlagCountry == sub$FlagCountry &
          len_raise_noNA$gear == sub$gear &
          len_raise_noNA$quarter == sub$quarter,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])) {
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$LandingCountry == sub$LandingCountry &
          len_raise_noNA$gear == sub$gear &
          len_raise_noNA$quarter == sub$quarter,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])) {
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$LandingCountry == sub$LandingCountry &
          len_raise_noNA$gear == sub$gear,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])) {
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$LandingCountry == sub$LandingCountry &
          len_raise_noNA$gear == sub$gear,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])){
    len_freq <-
      len_raise_noNA[
        len_raise_noNA$Year == sub$Year &
          len_raise_noNA$gear == sub$gear,
        c("LengthClass", "SUM_NoAtLengthInCatch")
      ]
  }
  if (is.na(len_freq$LengthClass[1])) {
    warning(paste(i,"NAs remain - check why", sub$gear))
  } else {
    len_freq <-
      aggregate(SUM_NoAtLengthInCatch ~ LengthClass, len_freq, sum)
    len_freq$prop <-
      len_freq$SUM_NoAtLengthInCatch / sum(len_freq$SUM_NoAtLengthInCatch)
    len_freq$SUM_NoAtLengthInCatch <- NULL
    sub$LengthClass <- NULL
    sub$prop <- NULL

    #store and return
    sub <- cbind.data.frame(sub, len_freq)
    if (is.null(out)) {
      out <- sub
    } else {
      out <- rbind.data.frame(out, sub)
    }
  }
  sub <- NULL
}

# Combine filled in dataset with existing
len_raise_NA_order <- out[,names(len_raise_noNA)]
final <- rbind.data.frame(len_raise_NA_order, len_raise_noNA)
dim(final)
final[1, ]

# Apply the length frequencies to the catches
final$natlen <- final$prop*final$SumOfficialLandingCatchWeightInKg

# "final" is the length frequency at the most disaggregated level with gaps filled in
# Now summarise for length frequencies by quarter and gear type exclusively
lenfreqs      <- aggregate(natlen ~ Year + quarter + gear + LengthClass, final, sum)
lenfreqs_tot  <- aggregate(natlen ~ Year + quarter + gear, final, sum)
lenfreqs      <- merge(lenfreqs,lenfreqs_tot, by=c("Year","quarter","gear"))

# now combine with ALK
alk <- read.taf("data/ALK.csv")

agefreqs <-
  merge(
    lenfreqs, alk,
    by = c("Year", "quarter", "LengthClass"), all.x = TRUE
  )

head(agefreqs)

agefreqs$natage <- agefreqs$natlen.x * agefreqs$age_prop
agefreqs$prop <- agefreqs$natage / agefreqs$natlen.y

# aggregate now
agefreqs <-
  agefreqs %>%
  group_by(Year, gear, Age) %>%
  summarise(numbers = sum(natage)) %>%
  ungroup() %>%
  group_by(Year, gear) %>%
  mutate(totals = sum(numbers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = numbers / totals)

# Plot population ages vs gear age frequencies
# Extract population ages
assessmt_age <- assessmt$natage[assessmt$natage$`Beg/Mid`=="B",]
# ignore warnings
pop_age <-
  assessmt_age[
    ,
    c("Yr", names(assessmt_age)[!is.na(as.numeric(names(assessmt_age)))])
  ]

#extract length vector for year yr_idx from the assessment
vecpop <- pop_age[pop_age$Yr %in% yr_idx, -1]
vecpop <- as.vector(matrix(apply(vecpop, 1, function(x) x/sum(x)), nrow=1))
popdf <-
  cbind.data.frame(
    Year = rep(yr_idx, each = length(as.numeric(names(pop_age)[-1]))),
    Age = rep(as.numeric(names(pop_age)[-1]), times = length(yr_idx)),
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

# reshape dataset for ggplot
gg_agefreqs <- agefreqs[, c("Age", "Year", "gear", "prop")]
gg_agefreqs2 <- agefreqs[, c("Age", "Year", "gear", "freq")]
names(gg_agefreqs2) <- names(gg_agefreqs)
gg_agefreqs$cat  <- "caught_by_gear"
gg_agefreqs2$cat <- "population"
gg_agefreqs <- rbind.data.frame(gg_agefreqs, gg_agefreqs2)

# cumulative sum
gg_agefreqs <- gg_agefreqs[order(gg_agefreqs$Age), ]
gg_agefreqs$cumsum <-
  ave(
    gg_agefreqs$prop, gg_agefreqs$cat, gg_agefreqs$Year,
    gg_agefreqs$gear,
    FUN = cumsum
  )
gg_agefreqs$legend <-
  paste(gg_agefreqs$Year, gg_agefreqs$cat, sep = "_")

# plot age sampled vs age distribution of population
ggplot(gg_agefreqs, aes(x = Age, y = prop, col = legend)) +
  geom_line() +
  facet_wrap(~gear)
# plot cumulative lengths vs cumsum population
ggplot(gg_agefreqs, aes(x = Age, y = cumsum, col = legend)) +
  geom_line() +
  facet_wrap(~gear)
# plot cumulative lengths by gear vs each other
gg_agefreqs$legend <- paste(gg_agefreqs$gear, gg_agefreqs$Year, sep = "_")

ggplot(
  gg_agefreqs[gg_agefreqs$cat == "caught_by_gear", ],
  aes(x = Age, y = cumsum, col = legend)
) +
  geom_line()

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
