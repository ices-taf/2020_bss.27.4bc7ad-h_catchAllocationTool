# 2019_4017-19_TechnicalService
EU DGMARE Technical Service: Seabass Simulation Tool

to run this repo use:
```r
install.packages("icesTAF")
install.packages(icesTAF::deps(installed = FALSE))

taf.bootstrap()
sourceAll()
sourceTAF("shiny")
```

then to run the app run:

```r
library(shiny)
runApp("shiny")
```
