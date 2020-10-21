# Function to optimis fleet Fmults to take the specified catches
# When optimising fmults, repress is set to TRUE

# Objective function for optimising fmults (sum of squares)
objective_func <- function(fmults, gearcatch, age_data, selectivity_age, TimeStep) {
  gearcatch_pred <-
    gearCatches(fmults, age_data, selectivity_age, TimeStep, quick = TRUE)

  sum((gearcatch_pred - gearcatch)^2)
}


# function to compute catches by gear given a vector of F multipliers
# data, population numbers, recreational fishing mortality, discard selection
# discard proportion and natural mortality

gearCatches <- function(fmults, age_data, selectivity_age, TimeStep, quick = TRUE) {
  gears <- names(selectivity_age)[-1]
  nages <- 17
  ngears <- length(gears)

  # calculate Fs and Z
  fmort <- as.matrix(selectivity_age[gears]) * rep(fmults[1:ngears], each = nages)
  dismort <- age_data$Discard_Sel * fmults[ngears + 1]
  zmort <- apply(fmort, 1, sum) + dismort + (age_data$f_age_rec_2020 + age_data$M) / TimeStep

  projCatch <- colSums(age_data$N * (1 - exp(-zmort)) * age_data$weights_age * cbind(fmort, Discards = dismort) / zmort, na.rm = TRUE)

  if (quick) {
    return(projCatch)
  }

  landN <- age_data$N * (1 - exp(-zmort)) * fmort / zmort

  if (sum(projCatch[1:ngears]) == 0) {
    disN <- replace(landN, TRUE, 0)
    catchmort <- fmort
  } else {
    activeDisProp <- other_data$discard_prop[gears] * projCatch[gears] / sum(projCatch[gears])

    disF <-
      outer(
        dismort,
        as.numeric(activeDisProp[gears] / sum(activeDisProp))
      )

    disN <- age_data$N * (1 - exp(-zmort)) * disF / zmort
    colnames(disN) <- gears
    catchmort <- fmort + disF
    gearDiscards <- colSums(disN * age_data$weights_age)
  }

  list(
    gearCatches = projCatch,
    gearDiscards = gearDiscards,
    catch_n = disN + landN,
    land_n = landN, dis_n = disN,
    catch_f = catchmort,
    land_f = fmort,
    dis_f = dismort,
    total_z = zmort
  )
}
