
# Additional functions
#source("utilities.R")

# Read in required data


source_data <- list()

## Initial population
source_data$pop_age_2020 <- read.csv("bootstrap/data/other/pop_age_2020.csv")

# H.Cons Retained mean weights from 'Bass47_STF 2019 assessment 20190514.xlsx'
#weights_age <- read.csv("data/weights_age.csv")
source_data$weights_age_rec <- read.csv("bootstrap/data/other/weights_age_Rec.csv")

source_data$noVessels <- read.taf("bootstrap/data/other/Number_Vessels.csv")

## Fleet selectivity by age
# assuming this is catch selectivity (i.e. L+D)
#selectivity_age <- read.csv("data/selectivity_age.csv")

# Use advice forecast discard F selectivity for all fleet (quantity of disards by fleet from discard proportion estimates, below)
#discard_Sel <- read.csv("data/discard_selectivity.csv")

## Discards
# discard proportions by gear (from last 3 years of French and English data)
# adjusted to match assumed total discard rate in the advice forecast
# no discard selectivites available, so projection simply uses gear landings selectivities
# discard proportion is used on the results to divide total catch by gear in landings and discards
# by gear (only C@A will be presented, no L@A or D@A)
#discard_prop <- read.csv("data/discard_proportions.csv")

## Recreational fisheries
# F multipliers for bag limits and closed seasons
source_data$RecFs <- read.csv("bootstrap/data/BagLimitFs.csv")

# Fbar of recreational fishery in 2012
source_data$Fbar_rec_2012 <- 0.0604
# Fbar of recreational fishery in 2019
source_data$Fbar_rec_2019 <- 0.018829873
# F at age of recreational fishery in 2019
source_data$F_age_rec_2019 <- c(
  0, 0.000205609, 0.00118539, 0.001176412, 0.006351395, 0.013477775, 0.012815346,
  0.023498693, 0.014668892, 0.022002183, 0.020376389, 0.023425403, 0.023519036,
  0.021145238, 0.021742123, 0.022936007, 0.024790632
)

# Z at age from the ICES advice forecasts
# This is used to estimate total Recreational catch
source_data$Advice_Z_age_2020 <- read.csv("bootstrap/data/other/ICESadvice_Forecast_Zs.csv")

# Forecasted Catch at age
#AdviceForecastCatchAge <- read.csv("data/AdviceForecastCatchAge.csv")


## Natural mortality
# per year
#Myr <- 0.24
# per Month
#M <- Myr / 12

## ICES advice (http://ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/bss.27.4bc7ad-h.pdf)
# Options for MAP
source_data$ICESadvMSY <- 1946
source_data$ICESadvMSYlow <- 1634
# Recreational catches from catch scenario (not used as a limit, just for comparison)
#ICESadvMSYRec <- 412
#ICESadvMSYlowRec <- 346

library(icesTAF)
library(dplyr)
library(rhandsontable)
library(glue)

# setup inputs
setup_input <- function(TimeStep = 12, ICESadvComm = 1, AdviceType = "MSY",
  Comm_v_Rec = 1, OpenSeason = 2, BagLimit = 2) {

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
    totZ <- source_data$Advice_Z_age_2020[, "MSY_Z"]
  } else {
    ICESadv <- source_data$ICESadvMSYlow
    totZ <- source_data$Advice_Z_age_2020[, "MSYlow_Z"]
  }

  # Get recreational F multiplier
  RecF <- source_data$RecFs[OpenSeason, BagLimit + 1]

  ## calculate recreational F based on management measures
  # uses selected multiplier and 2012+2019 F@A and Fbar to estimate 2020 F@A
  f_age_rec_2020 <-
    cbind.data.frame(
      Age = source_data$weights_age_rec$Age[1:length(source_data$F_age_rec_2019)],
      f_age_rec_2020 = RecF * source_data$F_age_rec_2019 * source_data$Fbar_rec_2012 / source_data$Fbar_rec_2019
    )

  # Mean F for recreational ages 4-15
  FbarRec <- mean(f_age_rec_2020[5:16, 2])

  # Monthly F values to use as estimates of recreational mortality in the monthly forecast (final rec catch values determined from annually below)
  f_age_rec_2020_month <- f_age_rec_2020
  f_age_rec_2020_month[, 2] <- f_age_rec_2020_month[, 2] / 12

  # Get recreational catch at age and total catch
  catchRec_n <- source_data$pop_age_2020[, "N"] * (1 - exp(-totZ)) * ((f_age_rec_2020[, 2]) / totZ)
  recCatch <- sum(catchRec_n * source_data$weights_age_rec[, 2])

  # Calculate what is left for the commercial fleets
  # DM: changes here. Currently the advice will be overshot if they allocate too much catch to the commercial without increasing the restrictions on the recreational catch
  if (Comm_v_Rec == 1) {
    ICESadvComm <- ICESadv - recCatch
  } else {
    ICESadvComm <- ICESadv
  }

  # return environment as list
  as.list(as.environment(-1L))
}


## process
calc_total <- function(data) {
  # if a hot table, extract the data from it
  if (inherits("table", "rhandsontable")) {
    data <- hot_to_r(data)
  }

  # add total row
  data["TOTAL", ] <- colSums(data[setdiff(rownames(data), "TOTAL"), ], na.rm = TRUE)
  data
}

calc_tonnes <- function(data, noVessels) {
  data * noVessels[rep(1, nrow(data)), names(data)]
}


## formating for ui
fmt_table <- function(data) {
  data <- calc_total(data)
  rhandsontable(data, rowHeaderWidth = 90, colWidths = 119) %>%
    hot_row(nrow(data), readOnly = TRUE)
}



remaining_quota <- function(leftOver, Comm_v_Rec, recCatch) {
  # different text depednign on whether rec or comm goes first
  if (Comm_v_Rec == 1) {
    col <- if (leftOver < 0) "red" else "green"
    glue("Quota remaining: <span style=\"color:{col}\"> {leftOver} t </span>")
  } else {
    col <- if (leftOver - recCatch < 0) "red" else "green"
    glue("Quota remaining: <span style=\"color:{col}\"> {leftOver} t </span>; with {round(recCatch,0)} t expected to be landed by recreational fishers given the chosen options")
  }
}

recreationalF <- function(recCatch, FbarRec) {
  glue(
    "For the options above, {round(recCatch, 0)} t of fish (F = {round(FbarRec, 3)}) will be ",
    "removed by the recreational fishery (through catches or mortality after catch-and-release)."
  )
}

ICESadvice <- function(ICESadv) {
  glue("The initial advice is {ICESadv} t")
}

ICESadviceCommercial <- function(Comm_v_Rec, ICESadvComm) {
  if (input$Comm_v_Rec == 1) {
    glue(" Remaining available catch is = {round(ICESadvComm, 0)} t.")
  } else {
    glue(
      " Remaining available catch is = {round(ICESadvComm, 0)} t; with {round(recCatch, 0)} t ",
      "expected to be landed by recreational fishers given the chosen options"
    )
  }
}

# summaries of forecasts
vclsGearTable <- function(TimeStep, vclsGearTable) {
  if (input$TimeStep == 12) {
    vclsGearTable
  } else {
    rbind(
      c(
        "Average monthly catch/vessel",
        as.character(round(as.numeric(vclsGearTable[13, -1]) / 12, 2))
      ),
      vclsGearTable[13, ]
    )
  }
}

# Output to get a dynamic output table using the time step
CatchGearTable <- function(TimeStep, CatchGearTable) {
  if (TimeStep == 12) {
    CatchGearTable
  } else {
    CatchGearTable[c(13, 14), ]
  }
}

fmt_forecast_table <- function(forecastTable) {
  # Output forecast table (table 3)
  DT::datatable(forecastTable, options = list(dom = "t")) %>%
  formatStyle(
    "Basis",
    target = "row",
    color = styleEqual("Simulated Scenario", "white"),
    backgroundColor = styleEqual("Simulated Scenario", "#dd4814")
  )
}


# ------ GO

input <- setup_input(12)

# some more inputs
input$data[] <- 0.05

# and calculate total tonnes per year
input$catchGear <- calc_total(calc_tonnes(input$data, input$noVessels))

# end of inputs

# summaries of inputs

fmt_table(input$data)
leftOver <- round(input$ICESadvComm - sum(calc_tonnes(input$data, input$noVessels), na.rm = TRUE), 0)
remaining_quota(leftOver, input$Comm_v_Rec, input$recCatch)
recreationalF(input$recCatch, input$FbarRec)
ICESadvice(input$ICESadv)
ICESadviceCommercial(input$Comm_v_Rec, input$ICESadvComm)

# run forecast
Forecast <-
  with(
    input,
    runForecast(
      months = month.abb,
      selectivity_age = selectivity_age,
      weights_age = weights_age,
      M = M,
      pop_age_2020 = pop_age_2020,
      f_age_rec_2020 = f_age_rec_2020,
      f_age_rec_2020_month = f_age_rec_2020_month,
      discard_prop = discard_prop,
      discard_Sel = discard_Sel,
      catches = catches,
      Monthly = Monthly,
      ICESadvComm = ICESadvComm,
      ICESadv = ICESadv,
      ICESadvOpt = ICESadvOpt,
      Myr = Myr,
      CatchGear = CatchGear,
      recCatch = recCatch,
      catchRec_n = catchRec_n,
      FbarRec = FbarRec,
      AdviceForecastCatchAge = AdviceForecastCatchAge,
      noVessels = noVessels
    )
  )


# summaries of forecast
Forecast$catchNplot
CatchGearTable(input$TimeStep, Forecast$CatchGearTable)
vclsGearTable(input$TimeStep, Forecast$vclsGearTable)
fmt_forecast_table(Forecast$forecastTable)
