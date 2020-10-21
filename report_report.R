

if (FALSE) {

  # summaries of inputs
  load("model/input.RData")

  fmt_table(input$data)

  leftOver <- round(input$ICESadvComm - sum(calc_tonnes(input$data, input$noVessels), na.rm = TRUE), 0)
  remaining_quota(leftOver, input$Comm_v_Rec, input$recCatch)
  recreationalF(input$recCatch, input$FbarRec)
  ICESadvice(input$ICESadv)
  ICESadviceCommercial(input$Comm_v_Rec, input$ICESadvComm)


  load("model/forecast.RData")

  # summaries of forecast
  Forecast$catchNplot
  CatchGearTable(input$TimeStep, Forecast$CatchGearTable)
  vclsGearTable(input$TimeStep, Forecast$vclsGearTable)
  fmt_forecast_table(Forecast$forecastTable)
}
