
shinyUI(fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel(
    "Market Basket Matrix Tool",
  ),
  # tags$head(tags$style(type="text/css", "
  #            #loadmessage {
  #              position: fixed;
  #              top: 0px;
  #              left: 0px;
  #              width: 100%;
  #              padding: 5px 0px 5px 0px;
  #              text-align: center;
  #              font-weight: bold;
  #              font-size: 100%;
  #              color: #ffffff;
  #              background-color: #0067bf;
  #              z-index: 105;
  #            }
  #         ")),
  # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
  #                  tags$div("Loading...",id="loadmessage")),
  # fileInput("file", NULL, accept = c(".csv")),
  # h3(id = "sent", "All dropdowns in blue will affect the number of observations, which in turn affects lift calculations."),
  # tags$style("#sent", "{color:#0067bf}"),
  # fluidRow(
  #   column(4,
  #          pickerInput("selecttime",
  #                      shiny::HTML("<p><span style='color: #0067bf'>Filter Qtrs</span></p>"),
  #                      choices = "Upload File", multiple = TRUE,
  #                      options = list(`actions-box` = TRUE)),
  #          numericInput("selectbk",
  #                       shiny::HTML("<p><span style='color: #0067bf'>Bookings Minimum</span></p>"),
  #                       min = -1000000,
  #                       max = 1000000,
  #                       value = 1,
  #                       step = 5000)),
  #   column(4,
  #          pickerInput("selectvar",
  #                      shiny::HTML("<p><span style='color: #0067bf'>Select Filter</span></p>"),
  #                      choices = "Upload File"),
  #          pickerInput("selectp",
  #                      shiny::HTML("<p><span style='color: #0067bf'>Filter Values</span></p>"),
  #                      choices = "Upload File",
  #                      multiple = TRUE,
  #                      options = list(`actions-box` = TRUE)),
  #          pickerInput("selectobs",
  #                      shiny::HTML("<p><span style='color: #0067bf'>What is an Observation?</span></p>"),
  #                      choices = "Upload File",
  #                      multiple = TRUE)),
  #   column(4,
  #          pickerInput("selectintvar", "Variable of Interest (must select initially)", "Upload File"),
  #          pickerInput("selectvarvals", "Filter Variable of Interest", choices = "Upload File",
  #                      options = list(`actions-box` = TRUE, `live-search` = TRUE), multiple = TRUE),
  #          actionBttn("calc", "Refresh Plot", color = "success", icon = icon("th")))
  # ),

  tags$h4("Numbers in gray boxes represent # of observations (determined by What is an Observation?) for the given column"),
  tags$h3("Confidence"),
  plotOutput("plot1", height = "700px"),
  tags$h3("Lift"),
  tags$style("#transactions {font-size:20px; color:#0067bf}"),
  textOutput("transactions"),
  plotOutput("plot2", height = "700px"),
  downloadButton("downloadData", "Download Chart Data"),
  downloadButton("downloadData2", "Download Obs Data")

)
)

