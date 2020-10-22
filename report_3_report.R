

library(icesTAF)
library(rmarkdown)


mkdir("report")

render(
  "report.Rmd",
  output_dir = "report",
  params =
    list(
      setup_react = setup_react, forecast = forecast,
      summary = forecast_summary, other_data = other_data
    )
)
