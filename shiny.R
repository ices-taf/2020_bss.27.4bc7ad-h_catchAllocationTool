## Collect data, web files, and create the shiny app

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("shiny")

# create shiny app data folder
mkdir("shiny/data")

# copy in required data
cp("data/age_data.csv", "shiny/data")
cp("data/gear_selectivity_age.csv", "shiny/data")
cp("data/other_data.RData", "shiny/data")

# copy in utilities
cp(
  c("utilities_shiny.R", "utilities_setup_input.R",
  "utilities_gearCatches.R", "utilities_forecast.R"),
  "shiny"
)

# copy over www folder
mkdir("shiny/www")
cp(taf.data.path("www"), "shiny")

# copy markdown pages
cp("shiny_Introduction.Rmd", "shiny/Introduction.Rmd")
cp("shiny_Instructions.Rmd", "shiny/Instructions.Rmd")
cp("shiny_UsefulLinks.Rmd", "shiny/UsefulLinks.Rmd")

# copy pdf
cp("shiny_TechnicalService.pdf", "shiny/www/TechnicalService.pdf")

# copy in server and ui scripts
cp("shiny_ui.R", "shiny/ui.R")
cp("shiny_server.R", "shiny/server.R")

msg("Created shiny app. To run, use: \n\n\trunApp('shiny')\n\n")
