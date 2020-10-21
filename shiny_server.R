
# Libraries
require(dplyr)
require(ggplot2)
require(shiny)
require(rhandsontable)
require(markdown)
require(shinythemes)
require(DT)

# load base data
age_data <- read.taf("data/age_data.csv")
load("data/other_data.RData")

# Additional functions
source("utilities_setup_input.R")
source("utilities_shiny.R")


server <- function(input, output) {

  cat(names(isolate(input)))

  # Dynamic input sections - where the input form changes
  # depending on the state.  Static inputs go in the ui.

  userInputs <- reactive({
    setup <-
        setup_input(
          TimeStep = input$TimeStep, ICESadvComm = input$ICESadvComm,
          AdviceType = input$AdviceType, Comm_v_Rec = input$Comm_v_Rec,
          OpenSeason = input$OpenSeason, BagLimit = input$BagLimit,
          source_data = other_data
        )
  })

  # changes in numericInput sets all (!) new values
  observe({
    req(input$table)
    userInputs$data <- calc_total(input$table)
    # and calculate total tonnes per year
    userInputs$setup$catchGear <- calc_tonnes(userInputs$data, userInputs$setup$noVessels)
  })

  output$table <-
    renderRHandsontable({
      req(userInputs$data)

      fmt_table(userInputs$data)
    })

  ## Line that calculates the remaining of catches after the recreational options
  output$RemQuota <-
    renderText({
      leftOver <- round(userInputs$setup$ICESadvComm - sum(calc_tonnes(userInputs$data, userInputs$setup$noVessels), na.rm = TRUE), 0)
      remaining_quota(leftOver, userInputs$setup$Comm_v_Rec, userInputs$setup$recCatch)
  })

  reactiveForecast <-
    eventReactive(
      input$go,
      {

      }
    )

  # the correct value for recreational F depending on the selections made
  output$RecF <-
    renderText({
      recreationalF(userInputs$setup$recCatch, userInputs$setup$FbarRec)
    })

  output$plot <-
    renderPlot({
      forecast()$catchNplot
    })


  # Output to get a dynamic output table using the time step
  output$CatchGearTable <-
    renderTable({
      req(input$TimeStep)

      CatchGearTable(input$TimeStep, forecast()$CatchGearTable)
    })

  output$vclsGearTable <-
    renderTable({
      req(input$TimeStep)

      vclsGearTable(input$TimeStep, forecast()$vclsGearTable)
    })

  # Output forecast table (table 3)
  output$forecastTable <-
    DT::renderDataTable(
      fmt_forecast_table(forecast()$forecastTable)
    )

  # output the value for the advice type choosen
  output$ICESadv <-
    renderText({
      ICESadvice(userInputs$setup$ICESadv)
    })

  # Output for the total amont available after the recreational selection
  #DM: trying to change the text below depending on whether rec or Comm catch taken first
  output$ICESadvComm <-
    renderText({
      ICESadviceCommercial(userInputs$setup$Comm_v_Rec, userInputs$setup$ICESadvComm)
    })

  # hiding wellPanels
  output$hide_panel <- eventReactive(input$go, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
}
