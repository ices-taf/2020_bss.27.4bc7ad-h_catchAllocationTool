

## process
calc_total <- function(data) {
  # if a hot table, extract the data from it
  if (inherits(data, "rhandsontable")) {
    data <- hot_to_r(data)
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # strip off total
  if ("TOTAL" %in% rownames(data)) {
    data <- data[-grep("TOTAL", rownames(data)), ]
  }

  # set row names
  if (nrow(data) == 12) {
    rownames(data) <- month.name
  } else if (nrow(data) == 1) {
    rownames(data) <- "Annual"
  }

  # add total row
  rbind(data, TOTAL = colSums(data, na.rm = TRUE))
}

# calculate total catch by gear
calc_tonnes <- function(data, noVessels) {
  data * noVessels[rep(1, nrow(data)), names(data)]
}

calc_tonnes_by_vessel <- function(data, noVessels) {
  data / noVessels[rep(1, nrow(data)), names(data)]
}



## formating for ui
fmt_table <- function(data, total = FALSE) {

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

ICESadviceCommercial <- function(Comm_v_Rec, ICESadvComm, recCatch) {
  if (Comm_v_Rec == 1) {
    glue(" Remaining available catch is = {round(ICESadvComm, 0)} t.")
  } else {
    glue(
      " Remaining available catch is = {round(ICESadvComm, 0)} t; with {round(recCatch, 0)} t ",
      "expected to be landed by recreational fishers given the chosen options"
    )
  }
}
