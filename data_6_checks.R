
library(icesTAF)

age_data <- read.taf("data/age_data.csv")

data_check <- read.taf(taf.data.path("checks", "forecast_inputs.csv"))

comp <- intersect(names(data_check), names(age_data))

tmp <-
  age_data[comp] %>%
    rnd(c("N_2020"), 0) %>%
    rnd("[^N_2020]", 3, grep = TRUE)

diff <- data_check[comp] - tmp

cat("data check on forecast inputs:\n\n")
print(diff)

if (any(abs(diff) > 1e3)) {
  warning("some large differences in forecast inputs")
}
