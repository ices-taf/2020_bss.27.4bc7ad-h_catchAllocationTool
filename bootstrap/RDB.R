#' seabasss data from the Regional Data Base (RDB)
#'
#' seabasss data from the Regional Data Base (RDB).  Age and length,
#' landings and length data are included as seperate files.
#'
#' @name RDB
#' @format csv files
#' @tafOriginator ICES, WGCSE
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)
library(readr)
taf.library(icesSharePoint)

spgetfile(
  "Internal Administration/datacall_files.zip",
  "/Admin/Requests/dgmare_seabass_tool",
  "https://community.ices.dk",
  destdir = "."
)

unzip("datacall_files.zip")
unlink("datacall_files.zip")

land <-
  read_csv(
    taf.data.path("RDB", "RDB seabass Landings data_.csv"),
    na = c("", "NA", "NULL"),
    col_types = cols()
  )

# Change landings table variables to match lengths variables
land$LandingCountry[land$LandingCountry=="ENG"] <- "England"
land$LandingCountry[land$LandingCountry=="WLS"] <- "Wales"
land$LandingCountry[land$LandingCountry=="FRA"] <- "France"
land$LandingCountry[land$LandingCountry=="NLD"] <- "Netherlands"
land$LandingCountry[land$LandingCountry=="NIR"] <- "Northern Ireland"
land$LandingCountry[land$LandingCountry == "SCT"] <- "Scotland"
land$LandingCountry[land$LandingCountry == "BEL"] <- "Belgium"
land$LandingCountry[land$LandingCountry == "GBR"] <- "Great Britain"
land$FlagCountry[land$FlagCountry=="ENG"] <- "England"
land$FlagCountry[land$FlagCountry=="WLS"] <- "Wales"
land$FlagCountry[land$FlagCountry=="FRA"] <- "France"
land$FlagCountry[land$FlagCountry=="NLD"] <- "Netherlands"
land$FlagCountry[land$FlagCountry=="NIR"] <- "Northern Ireland"
land$FlagCountry[land$FlagCountry=="SCT"] <- "Scotland"

write.taf(land, file = "RDB seabass Landings data_.csv")
