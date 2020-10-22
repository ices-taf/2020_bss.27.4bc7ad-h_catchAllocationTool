
# save a global setting file for use in other places

mkdir("data")

# year
globals <-
  list(
    # years for selectivity calcs
    yr_idx = c(2017, 2018),
    ages = 0:16,
    current_year = 2020
  )


save(globals, file = "data/globals.RData")
