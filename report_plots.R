

library(ggplot2)
library(dplyr)
library(plotly)

source("./utilities_forecast.R")

load("data/globals.RData")


p <- catch_n_plot(forecast, input)

ggplotly(p)
