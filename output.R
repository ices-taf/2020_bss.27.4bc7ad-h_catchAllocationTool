## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(icesAdvice)

mkdir("output")

load("model/input.RData")
age_data <- input$age_data

load("model/forecast.RData")
out <- forecast$out


gears <- names(input$data)


# Commercial Landings
realisedLandings <- do.call(rbind, lapply(out, "[[", "gearCatches"))[,gears]
totCommLandings <- sum(realisedLandings)

# Commercial Discards
realisedDiscards <- do.call(rbind, lapply(out, "[[", "gearDiscards"))[, gears]
totCommDiscards <- sum(realisedDiscards)

# Commercial Catch
realisedCommCatch <- realisedLandings + realisedDiscards
totCommCatch <- sum(realisedCommCatch)

# to make perfect with advice
if (sum(catches, na.rm = TRUE) > input$ICESadvComm) {
  adj <- input$ICESadvComm / totCommCatch
  totCommCatch <- input$ICESadvComm
  totCommLandings <- adj * totCommLandings
  totCommDiscards <- adj * totCommDiscards
  realisedCommCatch <- adj * realisedCommCatch
}

# Catch including recreational
realisedCatch <-
  calc_total(cbind(realisedCommCatch, Recreational = NA))
realisedCatch["TOTAL", "Recreational"] <- input$recCatch

totalCatch <- totCommCatch + input$recCatch


# Catch at age
catch_n <-
  cbind(
    Reduce("+", lapply(out, "[[", "catch_n")),
    Recreational = age_data$catchRec
  )

### F values
## Total
totalF <- Reduce("+", lapply(out, "[[", "total_z")) - age_data$M
Ftotbar <- icesRound(mean(totalF[5:16])) # ages 4-15


## Commercial F and Fbar
catchF <- Reduce("+", lapply(out, "[[", "catch_f"))
Fcomm <- rowSums(catchF) # F landings + discards
Fcommbar <- icesRound(mean(Fcomm[5:16]))
## By gear
gearFTable <- colMeans(catchF[5:16, ])
gearFTable[] <- icesRound(gearFTable)


## Landings
landF <- Reduce("+", lapply(out, "[[", "land_f"))
Fland <- rowSums(landF) # F landings
Flandbar <- icesRound(mean(Fland[5:16]))


## Discards
Fdis <- Reduce("+", lapply(out, "[[", "dis_f"))
Fdisbar <- icesRound(mean(Fdis[5:16]))

## Annual recreational catch and F
# recCatch
FbarRec <- icesRound(input$FbarRec)

# Change ages for Jan 2021
initPop[, ncol(initPop)] <-
  c(
    0,
    initPop[-nrow(initPop) + 0:1, ncol(initPop)],
    initPop[nrow(initPop), ncol(initPop)] + initPop[nrow(initPop) - 1, ncol(initPop)]
  )

# SSB 2021
ssb2021 <- sum(initPop[, ncol(initPop)] * age_data$mat * age_data$stkwt)
