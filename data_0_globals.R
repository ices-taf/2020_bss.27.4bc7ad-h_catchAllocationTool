
# save a global setting file for use in other places

mkdir("data")

# year
globals <-
  list(
    yr_idx = c(2018, 2019),
    ages = 0:16,
    current_year = 2020
  )


save(globals, file = "data/globals.RData")
