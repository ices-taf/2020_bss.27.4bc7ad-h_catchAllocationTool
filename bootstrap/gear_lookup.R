#' Lookup table of gear groupings
#'
#' Lookup table of gear groupings
#'
#' @name gear_lookup
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

# Group metiers of interest
gear_lookup <-
  list(
    "Gill Nets" = c("GN", "GNS", "GTR", "GND", "FYK"),
    "Hooks and Lines" = c("LHM", "LHP", "LLS", "LLD", "LTL", "LX"),
    "Pelagic Trawl" = c("PTB", "PTM"),
    "Demersal Trawl" = c("TBB", "OTB", "OTM", "OTT"),
    Seines = c("SSC", "SDN", "SB"),
    "Purse Seine" = "PS",
    Other = c("MIS", "DRB", "FPO", "LA", "HMD", "No")
  )

gear_lookup <-
  data.frame(
    Metierlvl4 = unname(unlist(gear_lookup)),
    gear = rep(names(gear_lookup), sapply(gear_lookup, length))
  )

write.taf(gear_lookup)
