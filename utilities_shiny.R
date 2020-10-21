

## process
calc_total <- function(data) {
  # if a hot table, extract the data from it
  if (inherits("table", "rhandsontable")) {
    data <- hot_to_r(data)
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  rownames(data) <- month.name

  # add total row
  data["TOTAL", ] <- colSums(data[setdiff(rownames(data), "TOTAL"), ], na.rm = TRUE)
  data
}

# calculate total catch by gear
calc_tonnes <- function(data, noVessels) {
  data * noVessels[rep(1, nrow(data)), names(data)]
}

calc_tonnes_by_vessel <- function(data, noVessels) {
  data / noVessels[rep(1, nrow(data)), names(data)]
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
