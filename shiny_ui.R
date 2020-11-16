require(shiny)
require(rhandsontable)
require(shinythemes)
require(markdown)
require(dplyr)
require(ggplot2)
require(plotly)
require(icesTAF)
require(tidyr)
require(glue)

side_width <- 5

# allocations input panel
allocations_inputpanel <-
  sidebarPanel(
    width = side_width,
    radioButtons(
      "AdviceType",
      label = h4("Select 2020 catch advice"),
      choiceNames =
        list(
          HTML("EU MAP F<sub>MSY</sub>"),
          HTML("EU MAP F<sub>MSY lower</sub>")
        ),
      choiceValues = list("MSY", "MSYlow"),
      inline = TRUE,
      selected = "MSY"
    ),

    h5(textOutput("ICESadv")),

    hr(),
    radioButtons(
      "TimeStep",
      label = h4("Select time step"),
      choices = list("Annual" = 1, "Monthly" = 12),
      inline = TRUE,
      selected = 1
    ),

    hr(),
    h4(helpText(" Select recreational management measures.")),

    selectInput(
      "OpenSeason",
      label = shiny::div(style = "font-size:13px", "Duration of open season"),
      choices = c(
        "0 months" = 1,
        "3 months" = 2,
        "6 months" = 3,
        "7 months" = 4,
        "9 months" = 5,
        "10 months" = 6,
        "12 months" = 7
      ),
      width = "40%",
      selected = 5
    ),

    selectInput(
      "BagLimit",
      label = shiny::div(style = "font-size:13px", "Bag limit size"),
      choices = c(
        "1 Fish" = 1,
        "2 Fish" = 2,
        "3 Fish" = 3,
        "4 Fish" = 4,
        "5+ Fish" = 5
      ),
      width = "40%",
      selected = 2
    ),

    # removing for now - needs development
    #selectInput(
    #  "Comm_v_Rec",
    #  label = shiny::div(style = "font-size:13px", "Start allocations with:"),
    #  choices = c(
    #    "Recreational" = 1,
    #    "Commercial" = 2
    #  ),
    #  width = "40%",
    #  selected = 1
    #),

    h5(textOutput("RecF")),

    h5(textOutput("ICESadvComm")),

    hr(),
    h4(helpText("Input catch allocations (in tonnes per vessel)")),
    tableOutput("noVesselsTable"),
    br(),

    actionButton("go", "Run simulation"),
    rHandsontableOutput('table'),

    htmlOutput("RemQuota"),
    position = "left"
  )


# results side panel
allocations_resultspanel <-
  mainPanel(
    width = 12 - side_width,
    conditionalPanel(
      "output.hide_panel",
      wellPanel(
        plotlyOutput("plot"),
        h5(helpText("Figure 1: Simulated catch-at-age, by gear. The dashed line (---) indicates the predicted catch-at-age in the ICES forecast."))
      ),
      wellPanel(
        h5(helpText("Table 1: Simulated catch allocations by vessel. Catch allocations may be less than those entered since total catch is limited to the advice level chosen. Weights are in tonnes.")),
        br(),
        tableOutput("vclsGearTable")
      ),
      wellPanel(
        h5(helpText("Table 2: Simulated catch and F by gear raised to fleet-level, including recreational catches. Weights are in tonnes.")),
        br(),
        tableOutput("CatchGearTable")
      )
    )
  )

# full width results table
allocations_resultspanel_wide <-
  fluidRow(
    column(12,
      conditionalPanel("output.hide_panel",
        wellPanel(
          h5(helpText("Table 3: Forecast scenarios. Comparison between the simulated scenario (highlighted row) and the basis of ICES advice for 2020. Weights are in tonnes. Note that the '% Advice change' value is relative to the advice for the corresponding scenario for 2020: FMSY (1946 tonnes) and FMSY lower (1634 tonnes).")),
          br(),
          DT::dataTableOutput("forecastTable")
        )
      )
    )
  )


# user interface
ui <-
  navbarPage(
    # tab title
    windowTitle = "Sea bass catch allocation tool",

    # navbar title
    title =
      shiny::div(img(src = 'Sea_bass_Negative_LOGO.png',
              style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
              height = 60)),

  tabPanel(
      "Introduction",
      includeMarkdown("Introduction.Rmd")
    ),
    
  tabPanel(
    "Instructions",
    includeMarkdown("Instructions.Rmd")
  ),

  tabPanel(
    "Allocations",
    sidebarLayout(
      sidebarPanel = allocations_inputpanel,
      mainPanel = allocations_resultspanel
    ),
    allocations_resultspanel_wide
  ),

  tabPanel(
    "Useful links",
    includeMarkdown("UsefulLinks.Rmd")
  ),

  # extra tags, css etc
  tags$style(type = "text/css", "li {font-size: 17px;}"),
  tags$style(type = "text/css", "p {font-size: 18px;}"),
  tags$style(type = "text/css", "body {padding-top: 70px;}"),
  tags$head(tags$style(HTML('#go{background-color:#dd4814}'))),
  theme = shinytheme("united"),
  position = "fixed-top",

  tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-taf/2020_bss.27.4bc7ad-h_catchAllocationTool\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
    console.log(header)"))
)
