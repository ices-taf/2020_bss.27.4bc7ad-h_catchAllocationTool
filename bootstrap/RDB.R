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
taf.library(icesSharePoint)

spgetfile(
  "Internal Administration/datacall_files.zip",
  "/Admin/Requests/dgmare_seabass_tool",
  "https://community.ices.dk",
  destdir = "."
)

unzip("datacall_files.zip")
unlink("datacall_files.zip")
