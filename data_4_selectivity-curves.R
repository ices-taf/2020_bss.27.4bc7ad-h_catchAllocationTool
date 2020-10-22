
# Load libraries
library(ggplot2)
library(sicegar) # forselection curves
library(r4ss) # for rectreational selex
library(dplyr)
library(tidyr)

# year
load("data/globals.RData")

# Read in length frequencies
agefreqs <- read.taf("data/ages_for_selectivity.csv")
names(agefreqs) <- c("Yr","Age","gear","prop","freq","selex")

# Plot selectivity
agefreqs$Yr <- as.factor(agefreqs$Yr)
ggplot(agefreqs, aes(x = Age, y = selex, col = Yr)) +
  geom_line() +
  facet_wrap(~gear)

# Remove the plus group
agefreqs_cutoff <- agefreqs[agefreqs$Age < max(agefreqs$Age), ]
ggplot(agefreqs_cutoff, aes(x = Age, y = selex, col = Yr)) +
  geom_line() +
  facet_wrap(~gear)
# remove any ages above 20
agefreqs_cutoff <- agefreqs[agefreqs$Age <= 20, ]
ggplot(agefreqs_cutoff, aes(x = Age, y = selex, col = Yr)) +
  geom_line() +
  facet_wrap(~gear)
# Remove weird peak for hook and line
agefreqs_tamperedWith <-
  agefreqs_cutoff[agefreqs_cutoff$selex < max(agefreqs_cutoff$selex), ]
ggplot(agefreqs_tamperedWith, aes(x = Age, y = selex, col = Yr)) +
  geom_line() +
  facet_wrap(~gear)
# Remove 2018 for PelTrawl
agefreqs_tamperedWith <-
  agefreqs_tamperedWith[
    !(agefreqs_tamperedWith$gear == "Peltrawl" &
      agefreqs_tamperedWith$Yr == 2018),
  ]
# Remove 2017 for Seines
agefreqs_tamperedWith <-
  agefreqs_tamperedWith[
    !(agefreqs_tamperedWith$gear == "Seines" &
      agefreqs_tamperedWith$Yr == 2017),
  ]

# Choose final dataset
agefreqs_final <- agefreqs_tamperedWith

# Fit selectivity curves
to_save <- NULL
for (g_idx in unique(agefreqs_final$gear)) {

  # by gear type
  fit.data <- agefreqs_final[agefreqs_final$gear == g_idx, ]

  # using the sicegar package for double normal
  fit.data <- fit.data[, c("Age", "selex")]
  names(fit.data) <- c("time", "intensity")
  fitObj <-
    fitAndCategorize(
      fit.data,
      threshold_intensity_range = 0.00001,
      threshold_minimum_for_intensity_maximum = 0.00001
    )
  sicegar.fit <-
    figureModelCurves(
      dataInput = fitObj$normalizedInput,
      sigmoidalFitVector = fitObj$sigmoidalModel,
      doubleSigmoidalFitVector = fitObj$doubleSigmoidalModel,
      showParameterRelatedLines = TRUE
    )
  sicegar.fit$labels$y="Selectvity"
  sicegar.fit$labels$x="Length (cm)"
  # plot the double normal selectivities
  # g <- plot(sicegar.fit) + ggtitle(g_idx)
  # print(g)

  # Extract outputs
  fits_dblSig <-
    doublesigmoidalFitFormula(
      fit.data$time,
      finalAsymptoteIntensityRatio = fitObj$doubleSigmoidalModel$finalAsymptoteIntensityRatio_Estimate,
      maximum = fitObj$doubleSigmoidalModel$maximum_y,
      slope1Param = fitObj$doubleSigmoidalModel$slope1Param_Estimate,
      midPoint1Param = fitObj$doubleSigmoidalModel$midPoint1Param_Estimate,
      slope2Param = fitObj$doubleSigmoidalModel$slope2Param_Estimate,
      midPointDistanceParam = fitObj$doubleSigmoidalModel$midPointDistanceParam_Estimate
    )


  fits_sig <-
    sigmoidalFitFormula(
      fit.data$time,
      maximum = fitObj$sigmoidalModel$maximum_y,
      slopeParam = fitObj$sigmoidalModel$slopeParam_Estimate,
      midPoint = fitObj$sigmoidalModel$midPoint_Estimate
    )

  fit.data$gear <- g_idx
  fit.data$type = "data"
  fit.data$AIC <- NA
  fits_sig <-
    cbind.data.frame(
      time = fit.data$time, intensity = fits_sig, gear = g_idx,
      type = "sigfit", AIC = fitObj$sigmoidalModel$AIC_value
    )
  fits_dblSig <-
    cbind.data.frame(
      time = fit.data$time, intensity = fits_dblSig, gear = g_idx,
      type = "dblSigfit", AIC = fitObj$doubleSigmoidalModel$AIC_value
    )
  to_save <- rbind.data.frame(to_save, fit.data, fits_sig, fits_dblSig)
}

# save outputs
selex <- to_save
names(selex)[1:2] <- c("Age","Selectivity")

# Plot selectivity curves
p <- ggplot(selex, aes(x = Age, y = Selectivity, colour = type)) +
  geom_line() +
  facet_wrap(~gear)

dat_text <- unique(selex[selex$type != "data", -c(1, 2)])
dat_text$Age <- 10
dat_text$Selectivity[dat_text$type == "sigfit"] <- 0.0035
dat_text$Selectivity[dat_text$type == "dblSigfit"] <- 0.004
dat_text$label <- paste("AIC =", round(dat_text$AIC, 2))
p +
  geom_text(dat_text, mapping = aes(label = label))

# Choose selectivities going forward - based on shapes out of assessment rather than AIC
# logistics for pelTrawl and hooklines
# double sigmoid for all others
# extract selectivity for recreational fishery from assessment

selex_final <- selex[selex$type != "data", ]
selex_final <-
  selex_final[!(selex_final$gear == "Demersal Trawl" & selex_final$type == "dblSigfit"), ]
selex_final <-
  selex_final[!(selex_final$gear == "Gill Nets" & selex_final$type == "sigfit"), ]
selex_final <-
  selex_final[!(selex_final$gear == "Hooks and Lines" & selex_final$type == "dblSigfit"), ]
selex_final <-
  selex_final[!(selex_final$gear == "Other" & selex_final$type == "sigfit"), ]
# DM# Change to extract Seine selectivity
# selex_final <- selex_final[!(selex_final$gear %in% c("PelTrawl","Seines")),]
selex_final <-
  selex_final[!(selex_final$gear == "Seines" & selex_final$type == "sigfit"), ]
selex_final <-
  selex_final[!(selex_final$gear == "Pelagic Trawl"), ]


# load assessment results
load("data/assessmemt.RData")

# Use selex provided in output for last year of assessment data for recre et pel
selex_recre <- assessmt$ageselex[assessmt$ageselex$Fleet == 6 & assessmt$ageselex$Yr == 2019, ]
selex_recre <- selex_recre[selex_recre$Factor %in% "Asel2", ]
selex_recre <- selex_recre[1, c(8:ncol(selex_recre))]
selex_recre <-
  cbind.data.frame(
    Age = as.numeric(names(selex_recre)),
    Selectivity = as.vector(unlist(selex_recre))
  )
selex_recre$gear <- "Recreational"
selex_recre$type <- "assessmt_selex_2019"
selex_recre$AIC <- NA

selex_final <- rbind.data.frame(selex_final, selex_recre)

# DM# some changes: 'Fleet' to 'fleet', 'yr' to 'year
selex_pel <- assessmt$ageselex[assessmt$ageselex$fleet == 3 & assessmt$ageselex$year == 2019, ]
selex_pel <- selex_pel[selex_pel$factor %in% "Asel2", ]
selex_pel <- selex_pel[1, c(8:ncol(selex_pel))]
selex_pel <-
  cbind.data.frame(
    Age = as.numeric(names(selex_pel)),
    Selectivity = as.vector(unlist(selex_pel))
  )
selex_pel$gear <- "Pelagic Trawl"
selex_pel$type <- "assessmt_selex_2019"
selex_pel$AIC <- NA

selex_final <- rbind.data.frame(selex_final, selex_pel)
selex_final <- unique(selex_final)
# DM# Change to 16+
# selex_final <- selex_final[selex_final$Age<=20,]
selex_final <- selex_final[selex_final$Age <= 16, ]

# Scale everything to 100%
for (xx in unique(selex_final$gear)) {
  selex_final[selex_final$gear == xx, "Selectivity"] <-
    selex_final[selex_final$gear == xx, "Selectivity"] /
      sum(selex_final[selex_final$gear == xx, "Selectivity"])
}

# One more plot for final check
ggplot(selex_final, aes(x = Age, y = Selectivity, colour = type)) +
  geom_line() +
  facet_wrap(~gear) +
  geom_vline(xintercept = 5)

ggsave("data/final_selectivities_age.png")

# save in simple format for shiny app
gear_selectivity_age <-
  selex_final %>%
  select(Age, Selectivity, gear) %>%
  filter(
    gear %in% c("Demersal Trawl", "Gill Nets", "Hooks and Lines", "Seines")
  ) %>%
  pivot_wider(
    names_from = gear, values_from = Selectivity
  ) %>%
  arrange(Age)

write.taf(gear_selectivity_age, dir = "data")
