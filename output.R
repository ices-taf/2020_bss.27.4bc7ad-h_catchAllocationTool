## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(icesAdvice)

mkdir("output")

load("model/input.RData")

load("model/forecast.RData")
