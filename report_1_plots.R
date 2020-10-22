

library(ggplot2)
library(dplyr)
library(plotly)

source("utilities_forecast.R")
load("model/setup_react.RData")

p <- catch_n_plot(forecast, setup_react$setup)

ggsave("catch_numbers.png", path = "report", width = 10)

# ggplotly(p)
