require(shiny)
require(rhandsontable)
require(shinythemes)
require(markdown)
require(dplyr)
require(ggplot2)

side_width <- 5

# user interface
ui <- shiny::navbarPage(
  title =
    div(
      img(
        src = "Sea_bass_Negative_LOGO.png",
        style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
        height = 60
      )
    ),

  tabPanel(
    "Instructions",
    includeHTML("Instructions.Html")
  ),

  tabPanel(
    "Allocations",

    sidebarLayout(
      sidebarPanel(
        width = side_width,
        radioButtons("AdviceType",
          label = h4("Select 2020 catch advice"),
          choices = list(
            "EU MAP F<sub>MSY</sub>" = "MSY",
            "EU MAP Fmsy lower" = "MSYlow"
          ),
          inline = TRUE,
          selected = "MSY"
        ),

        h5(textOutput("ICESadv")),

        hr(),

        radioButtons("TimeStep",
          label = h4("Select time step"),
          choices = list("Annual" = 1, "Monthly" = 12),
          inline = TRUE,
          selected = 1
        ),

        hr(),

        h4(helpText(" Select recreational management measures.")),

        # SelectOpenSeason is coming from renderUI in server.r
        uiOutput("SelectOpenSeason"),

        # SelectBagLimit is coming from renderUI in server.r
        uiOutput("SelectBagLimit"),

        #DM: Comm_v_Rec is coming from renderUI in server.r
        uiOutput("Comm_v_Rec"),
        
        h5(textOutput("RecF")),

        h5(textOutput("ICESadvComm")),

        hr(),

        h4(helpText("Input catch allocations (in tonnes)")),

        actionButton("go", "Run simulation"),
        rHandsontableOutput("table"),

        htmlOutput("RemQuota"),

        position = "left"
      ),

      mainPanel(
        width = 12 - side_width,

        conditionalPanel(
          "output.hide_panel",

          wellPanel(
            plotOutput("plot"),
            h5(
              helpText(
                "Figure 1: Simulated catch-at-age, by gear.
                The dashed line (---) indicates the predicted catch-at-age in the ICES forecast."
              )
            )
          ),

          wellPanel(
            h5(
              helpText(
                "Table 1: Simulated catch allocations. Catch allocations may be less than those entered since total
                catch is limited to the advice level chosen."
              )
            ),

            br(),

            tableOutput("vclsGearTable")
          ),

          wellPanel(
            h5(
              helpText(
                "Table 2: Simulated catch and F by gear, including recreational catches."
              )
            ),

            br(),

            tableOutput("CatchGearTable")
          )
        )
      )
    ),

    fluidRow(
      column(
        12,
        conditionalPanel(
          "output.hide_panel",

          wellPanel(
            h5(
              helpText(
                "Table 3: Forecast scenarios. Comparison between the simulated scenario (highlighted row) and the basis of ICES advice for 2020."
              )
            ),

            br(),

            DT::dataTableOutput("forecastTable")
          )
        )

        # plotOutput("catch_plot"),
        # verbatimTextOutput("debug_text"),
        # verbatimTextOutput("debug_text_output")
      )
    )
  ),

  tabPanel(
    "Useful links",
    includeHTML("UsefulLinks.Html")
  ),

  tags$style(type = "text/css", "body {padding-top: 70px;}"),
  tags$head(tags$style(HTML("#go{background-color:#dd4814}"))),
  theme = shinytheme("united"),
  position = "fixed-top",
  tags$script(
    HTML(
      paste0(
        "var header = $('.navbar > .container-fluid');",
        "header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-tools-prod/seabass-catch-allocation-tool\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');",
        "console.log(header);"
      )
    )
  )
)
