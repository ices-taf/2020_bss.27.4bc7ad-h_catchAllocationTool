
# save assessment output
library(r4ss)

mkdir("data")

assessmt <-
  SS_output(
    taf.data.path("assessment"),
    covar = FALSE, readwt = FALSE, printstats = FALSE
  )

save(assessmt, file = "data/assessmemt.RData")
