run_forecast <- function(gear_catches, selectivity_age, input, other_data) {

  # Gear types
  gears <- names(input$data)

  # Population matrix (Jan 2020 to Jan 2021)
  initPop <- matrix(NA, 17, input$TimeStep + 1, dimnames = list(0:16, NULL))
  initPop[, 1] <- input$age_data$N

  # the forecast optimisation ---------
  # -----------------------------------

  out <- list()
  # Switch for whether there is quota left or not (starts TRUE, changes to FALSE when quota used up.
  # Rest of the months then have zero comm catch)
  quota_left <- TRUE
  catches <- gear_catches

  # loop over time steps
  for (i in 1:input$TimeStep) {
    if (input$TimeStep == 12) {
      # check how much of TAC has been taken
      if (quota_left) {
        caught <- 0
        if (i > 1) {
          for (ii in 1:(i - 1)) {
            caught <- caught + sum(out[[ii]]$gear_catches)
          }
        }
        remaining <- input$ICESadvComm - caught
        # if quota is exceeded, scale catches, and set remaining to zero
        if (sum(catches[i, ]) > remaining) {
          catches[i, ] <- catches[i, ] * (remaining / sum(catches[i, ]))
          if (i != 12) {
            for (ii in (i + 1):12) {
              catches[ii, ] <- 0
            }
          }
          quota_left <- FALSE
        }
      }
    }
    # Split catches in landings and discards
    discard_prop <- other_data$discard_prop[names(catches)]
    landings_and_discards <-
      c(
        unlist(catches[i, ] * (1 - discard_prop)),
        Discards = sum(catches[i, ] * discard_prop)
      )

    # optimise Fmults to take the catches specified
    opt <-
      optim(
        rep(0, length(landings_and_discards)),
        objective_func,
        gearcatch = landings_and_discards,
        age_data = input$age_data,
        selectivity_age = selectivity_age,
        TimeStep = input$TimeStep,
        other_data = other_data,
        lower = rep(0, length(landings_and_discards)),
        method = "L-BFGS-B"
      )

    # Use optimised fmults to get catch.n, commercial F and total Z
    forecast <-
      gearCatches(opt$par, input$age_data, selectivity_age, TimeStep = input$TimeStep, other_data = other_data, quick = FALSE)

    # Project population forward
    # Note, ages unchanged, for Jan2021 shifted one age older after this loop
    initPop[, i + 1] <- initPop[, i] * exp(-forecast$total_z)

    # Save monthly results in list
    out[[i]] <- forecast
  }

  # Change ages for Jan 2021
  initPop[, ncol(initPop)] <-
    c(
      0,
      initPop[-nrow(initPop) + 0:1, ncol(initPop)],
      initPop[nrow(initPop), ncol(initPop)] + initPop[nrow(initPop) - 1, ncol(initPop)]
    )

  list(out = out, catches = catches, initPop = initPop)
}




summarise_forecast <- function(forecast, input) {

  out <- forecast$out
  age_data <- input$age_data

  gears <- names(input$data)

  # Commercial Landings
  realisedLandings <- do.call(rbind, lapply(out, "[[", "gearCatches"))[, gears, drop = FALSE]
  totCommLandings <- sum(realisedLandings)

  # Commercial Discards
  realisedDiscards <- do.call(rbind, lapply(out, "[[", "gearDiscards"))[, gears, drop = FALSE]
  totCommDiscards <- sum(realisedDiscards)

  # Commercial Catch
  realisedCommCatch <- realisedLandings + realisedDiscards
  totCommCatch <- sum(realisedCommCatch)

  # to make perfect with advice
  if (sum(forecast$catches, na.rm = TRUE) > input$ICESadvComm) {
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

  # SSB 2021
  ssb2021 <- sum(forecast$initPop[, ncol(forecast$initPop)] * input$age_data$mat * input$age_data$stkwt)

  list(
    realisedLandings = realisedLandings,
    realisedDiscards = realisedDiscards,
    realisedCommCatch = realisedCommCatch,
    realisedCatch = realisedCatch,
    FbarRec = FbarRec,
    Ftotbar = Ftotbar
  )
}


catchGearTable <- function(forecast_summary) {

  out <-
    rbind(
      as.matrix(round(forecast_summary$realisedCatch)),
      "F" = c(as.numeric(forecast_summary$gearFTable), as.numeric(forecast_summary$FbarRec))
    )

  # Add total column
  out <-
    cbind(
      "Month" = row.names(out),
      out,
      TOTAL = rowSums(out, na.rm = TRUE)
    )
  out["F", "TOTAL"] <- forecast_summary$Ftotbar # to account for rounding errors

  if (nrow(out) < 12) {
    colnames(out)[1] <- "."
  }

  out
}



catch_n_plot <- function(forecast, input) {

  catch_n <-
    cbind(
      Reduce("+", lapply(forecast$out, "[[", "catch_n")),
      Recreational = input$age_data$catchRec
    )

  ## Catch at age plot
  catch_data <-
    cbind(Age = input$age_data$Age, as.data.frame(catch_n)) %>%
    pivot_longer(-Age, names_to = "Gear", values_to = "Catch") %>%
    mutate(Catch = round(Catch))

  catch_advice <-
    data.frame(
      Age = input$age_data$Age,
      Advice = round(input$age_data[[input$AdviceType]])
    )

  p <-
    ggplot() +
    geom_area(
      data = catch_data, position = "stack",
      aes(x = Age, y = Catch, fill = Gear)
    ) +
    geom_line(
      data = catch_advice, linetype = 2,
      aes(x = Age, y = Advice)
    ) +
    ylab("Catch-at-Age (thousands)") +
    theme(plot.background = element_rect(fill = "grey96")) +
    theme(legend.background = element_rect(fill = "grey96", size = 0.5, linetype = "solid")) +
    theme(panel.background = element_rect(fill = "grey96"))

  p
}
