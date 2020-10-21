# Given the settings in the form in the app, set up the data that does
# not require any modelling
#
setup_input <- function(TimeStep = 12, ICESadvComm = 1, AdviceType = "MSY",
                        Comm_v_Rec = 1, OpenSeason = 2, BagLimit = 2,
                        source_data) {
  stopifnot(TimeStep %in% c(1, 12) || length(TimeStep) == 1)

  data <-
    data.frame(
      sapply(
        c("Demersal Trawl", "Gill Nets", "Hooks and Lines", "Seines"),
        function(x) rep(NA_integer_, TimeStep),
        simplify = FALSE,
        USE.NAMES = TRUE
      ),
      check.names = FALSE
    )
  row.names(data) <- if (TimeStep == 12) month.name else "Year"

  ## Fleet size (no vessel by gear)
  noVessels <- source_data$noVessels

  # Get advice value and total Z from advcie forecast
  if (AdviceType == "MSY") {
    ICESadv <- source_data$ICESadvMSY
    totZ <- age_data$MSY_Z
  } else {
    ICESadv <- source_data$ICESadvMSYlow
    totZ <- age_data$MSYlow_Z
  }

  # Get recreational F multiplier
  RecF <- source_data$RecFs[OpenSeason, BagLimit + 1]

  ## calculate recreational F based on management measures
  # uses selected multiplier and 2012+2019 F@A and Fbar to estimate 2020 F@A
  age_data$f_age_rec_2020 <-
    RecF * source_data$Fbar_rec_2012 *
      age_data$F_age_rec_2019 / mean(age_data$F_age_rec_2019[5:16])

  # Mean F for recreational ages 4-15
  FbarRec <- mean(age_data$f_age_rec_2020[5:16])

  # Get recreational catch at age and total catch
  age_data$catchRec <- age_data$N * (1 - exp(-totZ)) * age_data$f_age_rec_2020 / totZ
  recCatch <- sum(age_data$catchRec * age_data$weights_age_rec)

  # Calculate what is left for the commercial fleets
  # DM: changes here. Currently the advice will be overshot if they allocate too much catch to the commercial without increasing the restrictions on the recreational catch
  if (Comm_v_Rec == 1) {
    ICESadvComm <- ICESadv - recCatch
  } else {
    ICESadvComm <- ICESadv
  }

  # return environment as list
  out <- as.list(as.environment(-1L))
  out[-grep("source_data", names(out))]
}
