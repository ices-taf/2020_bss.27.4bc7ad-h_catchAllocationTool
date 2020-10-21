
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
selectivity_age <- read.taf("data/gear_selectivity_age.csv")
load("data/other_data.RData")

# Additional functions
source("utilities_setup_input.R")
source("utilities_shiny.R")
source("utilities_forecast.R")
source("utilities_gearCatches.R")

server <- function(input, output) {

  # Dynamic input sections - where the input form changes
  # depending on the state.  Static inputs go in the ui.

  setup_react <- reactiveValues(data = NULL, setup = NULL)

  observeEvent(
    {
      input$AdviceType
      input$TimeStep
      input$OpenSeason
      input$BagLimit
      input$Comm_v_Rec
    },
    {
      # register if timestep has changes
      timeStepChange <- setup_react$setup$TimeStep != input$TimeStep
      old_data <- setup_react$data

      # save new state
      setup <-
        setup_input(
          AdviceType = input$AdviceType, TimeStep = input$TimeStep,
          OpenSeason = input$OpenSeason, BagLimit = input$BagLimit,
          Comm_v_Rec = input$Comm_v_Rec,
          age_data = age_data, source_data = other_data
        )
      setup_react$setup <- setup


      # try set initial values
      if (!is.null(old_data)) {
        data <- setup_react$data
        if (timeStepChange) {
          if (input$TimeStep == 12) {
            # spread annual total through the year
            data <- old_data[rep(1, 12), ] / 12
          } else {
            # take annual total
            data <- calc_total(old_data[c(1, 13), ])
            data[] <- old_data[c(13, 13), ]
          }
        }
      } else {
        data <- setup$data
      }

      setup_react$data <- calc_total(data)
    }
  )

  # the correct value for recreational F depending on the selections made
  output$RecF <-
    renderText({
      recreationalF(setup_react$setup$recCatch, setup_react$setup$FbarRec)
    })

  observe(
    {
      req(input$table)
      data <- hot_to_r(input$table)
      setup_react$data <- calc_total(data)
    }
  )

  output$table <-
    renderRHandsontable({
      req(setup_react$data)
      fmt_table(setup_react$data)
    })

  ## Line that calculates the remaining of catches after the recreational options
  output$RemQuota <-
    renderText({
      leftOver <- round(setup_react$setup$ICESadvComm - sum(calc_tonnes(setup_react$data, setup_react$setup$noVessels), na.rm = TRUE), 0)
      remaining_quota(leftOver, setup_react$setup$Comm_v_Rec, setup_react$setup$recCatch)
  })

  # output the value for the advice type choosen
  output$ICESadv <-
    renderText({
      ICESadvice(setup_react$setup$ICESadv)
    })

  # Output for the total amont available after the recreational selection
  # DM: trying to change the text below depending on whether rec or Comm catch taken first
  output$ICESadvComm <-
    renderText({
      ICESadviceCommercial(setup_react$setup$Comm_v_Rec, setup_react$setup$ICESadvComm)
    })

  forecast_react <- reactiveValues(forecast = NULL)

  observeEvent(
    input$go,
    {
      gear_catches <- setup_react$data[-nrow(setup_react$data),]
      gear_catches[is.na(gear_catches)] <- 0
      gear_catches <- calc_tonnes(gear_catches, setup_react$setup$noVessels)
      forecast_react$forecast <- run_forecast(gear_catches, selectivity_age, setup_react$setup, other_data)
    }
  )




  output$plot <-
    renderPlotly({
      req(forecast_react$forecast)
      p <- catch_n_plot(forecast_react$forecast, setup_react$setup)
      ggplotly(p)
    })

if (FALSE) {

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
}

  # hiding wellPanels
  output$hide_panel <- eventReactive(input$go, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
}
