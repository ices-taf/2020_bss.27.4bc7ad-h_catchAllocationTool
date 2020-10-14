## Collect data, web files, and create the shiny app

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("shiny")

# create shiny app data folder
mkdir("shiny/data")

# copy in required data
cp(taf.data.path("BagLimitFs.csv"), "shiny/data")

# other data
# - try and reduce the copy pasting in the creation of these
cp(taf.data.path("other/*"), "shiny/data")

# copy over www folder
mkdir("shiny/www")
cp(taf.data.path("www"), "shiny")

# build html docs
render(
  "./shiny_Instructions.Rmd",
  output_file = "Instructions.html", output_dir = "shiny"
)

render(
  "./shiny_UsefulLinks.Rmd",
  output_file = "UsefulLinks.html", output_dir = "shiny"
)

# copy in server and ui scripts
cp("shiny_server.R", "shiny/server.R")
cp("shiny_ui.R", "shiny/ui.R")
cp("utilities_shiny.R", "shiny/utilities.R")

msg("Created shiny app. To run, use: \n\n\trunApp('shiny')\n\n")
