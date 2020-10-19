
# save a global setting file for use in other places

mkdir("data")

# year
globals <-
  list(
    yr_idx = c(2017, 2018)
  )


save(globals, file = "data/globals.RData")
