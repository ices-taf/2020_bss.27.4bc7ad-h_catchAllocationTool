
# function to compute catches by gear given a vector of F multipliers
# data, population numbers, recreational fishing mortality, discard selection
# discard proportion and natural mortality

gearCatches <- function(fmults, dat, pop, Frec, disSel, disProp, M, repress = TRUE) {
  gears <- unique(dat$gear)

  fmort <- matrix(0, nrow = 17, ncol = length(gears), dimnames = list(c(0:15, "16+"), gears))
  dismort <- matrix(0, nrow = 17, ncol = 1, dimnames = list(c(0:15, "16+"), "DiscardsTotal"))
  for (gg in 1:length(gears)) {
    fmort[, gg] <- fmults[gg] * dat[dat$gear == gears[gg], "Selectivity"]
  }
  dismort <- fmults[length(gears) + 1] * disSel
  zmort <- apply(fmort, 1, sum) + dismort + Frec[, 2] + M

  projCatch <- matrix(0, nrow = 1, ncol = (length(gears) + 1), dimnames = list("catch", c(paste(gears, "Land", sep = "_"), "DiscardsTotal")))
  for (gg in 1:length(gears)) {
    projCatch[gg] <- sum((pop * (1 - exp(-zmort)) * dat[dat$gear == gears[gg], "Weight"]) * (fmort[, gg] / zmort), na.rm = T)
    # could use discard weights (only very slightly different from landings weights)
  }
  projCatch[1 + length(gears)] <- sum((pop * (1 - exp(-zmort)) * dat[dat$gear == gears[gg], "Weight"]) * (dismort / zmort), na.rm = T)

  if (repress) {
    return(projCatch)
  }

  landN <- matrix(0, nrow = 17, ncol = length(gears), dimnames = list(c(0:15, "16+"), gears))
  for (gg in 1:length(gears)) {
    landN[, gg] <- pop * (1 - exp(-zmort)) * (fmort[, gg] / zmort)
  }
  catchmort <- fmort
  disN <- matrix(0, nrow = 17, ncol = length(gears), dimnames = list(c(0:15, "16+"), gears))
  if (sum(projCatch[, 1:length(gears)]) != 0) {
    activeDisProp <- disProp
    activeDisProp[, 2] <- (disProp[, 2] * projCatch[, 1:length(gears)]) / sum(projCatch[, 1:length(gears)])
    for (gg in 1:length(gears)) {
      disN[, gg] <- pop * (1 - exp(-zmort)) * (((dismort * activeDisProp[activeDisProp[, "Gear"] == gears[gg], 2]) / sum(activeDisProp[, 2])) / zmort)
      catchmort[, gg] <- catchmort[, gg] + ((dismort * activeDisProp[activeDisProp[, "Gear"] == gears[gg], 2]) / sum(activeDisProp[, 2]))
    }
  } else {
    for (gg in 1:length(gears)) {
      disN[, gg] <- pop * 0
    }
  }

  list(
    gearCatches = projCatch,
    catch_n = disN + landN,
    land_n = landN, dis_n = disN,
    catch_f = catchmort,
    land_f = fmort,
    dis_f = dismort,
    total_z = zmort
  )
}
