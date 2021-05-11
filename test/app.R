library(shiny)
library(tidyverse)
library(scales)
library(shinyWidgets)
library(KimelFunctions)


ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel(
    "Market Basket Matrix Tool",
  ),
    tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #ffffff;
               background-color: #0067bf;
               z-index: 105;
             }
          ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
  fileInput("file", NULL, accept = c(".csv")),
  h3(id = "sent", "All dropdowns in blue will affect the number of observations, which in turn affects lift calculations."),
  tags$style("#sent", "{color:#0067bf}"),
  fluidRow(
    column(4,
           pickerInput("selecttime",
                       shiny::HTML("<p><span style='color: #0067bf'>Filter Qtrs</span></p>"),
                       choices = "Upload File", multiple = TRUE,
                       options = list(`actions-box` = TRUE)),
           numericInput("selectbk",
                        shiny::HTML("<p><span style='color: #0067bf'>Bookings Minimum</span></p>"),
                        min = -1000000,
                        max = 1000000,
                        value = 1,
                        step = 5000)),
    column(4,
           pickerInput("selectvar",
                       shiny::HTML("<p><span style='color: #0067bf'>Select Filter</span></p>"),
                       choices = "Upload File"),
           pickerInput("selectp",
                       shiny::HTML("<p><span style='color: #0067bf'>Filter Values</span></p>"),
                       choices = "Upload File",
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE)),
           pickerInput("selectobs",
                       shiny::HTML("<p><span style='color: #0067bf'>What is an Observation?</span></p>"),
                       choices = "Upload File",
                       multiple = TRUE)),
    column(4,
           pickerInput("selectintvar", "Variable of Interest (must select initially)", "Upload File"),
           pickerInput("selectvarvals", "Filter Variable of Interest", choices = "Upload File",
                       options = list(`actions-box` = TRUE, `live-search` = TRUE), multiple = TRUE),
           actionBttn("calc", "Refresh Plot", color = "success", icon = icon("th")))
  ),

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

server <- function(input, output, session) {
    options(shiny.maxRequestSize=1000*1024^2)

  data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    shinyFeedback::hideFeedback("file")
    if(ext == "csv"){
    testing <- vroom::vroom(input$file$datapath, delim = ",", n_max = 10)
      if(!("USD Booking Amount" %in% names(testing))){
    shinyFeedback::feedbackDanger("file", !("USD Booking Amount" %in% names(testing)), "Must Include USD Booking Amount")
    shiny::validate(need("USD Booking Amount" %in% names(testing), "Need USD Booking Amount"))
      } else if (!("Fiscal Qtr" %in% names(testing))){
        shinyFeedback::feedbackDanger("file", !("Fiscal Qtr" %in% names(testing)), "Must Include Fiscal Qtr")
        shiny::validate(need("Fiscal Qtr" %in% names(testing), "Fiscal Qtr"))
      }
    } else {
      shinyFeedback::feedbackDanger("file", TRUE, "Must be csv file")
    }


    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           shiny::validate("Invalid file; Please upload a .csv file"))

  })


  #Updating the selections based on the input dataset
  data_p <- reactive({
    req(!!sym(input$selectvar) != "Upload File")
    data() %>% distinct(!!sym(input$selectvar)) %>% pluck(1) %>% .[order(.)]
  })

  step1 <- reactive({
    req(!!sym(input$selectvar) != "Upload File")
    shinyFeedback::feedbackWarning("selectobs", (input$selectintvar %in% input$selectobs),
                                   "Cannot use Variable of Interest")
    req(!(input$selectintvar %in% input$selectobs))
    data() %>%
      filter(!!sym(input$selectvar) %in% input$selectp,
             `Fiscal Qtr` %in% input$selecttime) %>%
      group_by(!!!syms(input$selectobs), !!sym(input$selectintvar)) %>%
      summarise(bk = sum(`USD Booking Amount`, na.rm = TRUE)) %>%
      filter(bk >= input$selectbk) %>%
      mutate(bk = bk > 0)
    })


  step1a <- reactive({
    step1() %>%
      filter(!!sym(input$selectintvar) %in% input$selectvarvals) %>%
      arrange(!!sym(input$selectintvar))
  })

  step2a <- reactive({
    step1a() %>%
      pivot_wider(names_from = input$selectintvar, values_from = "bk", values_fill = FALSE)%>%
      left_join(step1a() %>% select(-bk)) %>%
      select(!!!syms(input$selectobs), !!sym(input$selectintvar), everything())
  })


  step2 <- reactive({
    step2a() %>%
      group_by(!!sym(input$selectintvar)) %>%
      summarise_if(is_logical, sum, na.rm = TRUE)
  })

  labs_text <- reactive({
    step2() %>%
      select(!!sym(input$selectintvar), unique(step1a() %>% pull(!!sym(input$selectintvar)))) %>%
      pivot_longer(-!!sym(input$selectintvar)) %>%
      filter(!!sym(input$selectintvar) == name)
  })

  output$plot1 <- renderPlot({
    input$calc

    isolate(step2() %>%
              select(!!sym(input$selectintvar), unique(step1a() %>% pull(!!sym(input$selectintvar)))) %>%
              mutate_if(is_integer, ~. / max(.)) %>%
              pivot_longer(-!!sym(input$selectintvar)) %>%
              mutate(value2 = paste0(round(value, 2) * 100, "%"),
                     value2 = if_else(!!sym(input$selectintvar) == name, NA_character_, value2),
                     value = if_else(!!sym(input$selectintvar) == name, NA_real_, value)) %>%
              ggplot(aes(name, !!sym(input$selectintvar))) +
              geom_tile(aes(fill= value)) +
              labs(title = "How Often Was a Product Purchased Given Another Product Was Purchased?",
                   x = "Given this was Purchased",
                   y = "How Often was this Purchased?") +
              scale_fill_continuous(labels = scales::percent_format()) +
              geom_text(data = labs_text(), aes(label = value), size = 7, nudge_y = .1) +
              geom_text(aes(label = value2), color = "white", size = 7) +
              theme_bw() +
              theme(legend.title = element_blank(),
                    text = element_text(size = 20),
                    axis.text.x = element_text(angle = 90, vjust = .5, hjust = .95)))
  })




    transa <- reactive({
      step1() %>% distinct(!!!syms(input$selectobs)) %>% nrow()
    })

    output$transactions <- renderText({paste(transa(), " Observations")})

    probs <- reactive({
      step2() %>%
      pivot_longer(-!!sym(input$selectintvar)) %>%
      filter(!!sym(input$selectintvar) == name) %>%
      mutate(prob = value / transa())
    })

    lift <- reactive({
      step2() %>%
      mutate_if(is_integer, ~./transa()) %>%
      select(!!sym(input$selectintvar), unique(step1a() %>% pull(!!sym(input$selectintvar)))) %>%
      pivot_longer(-!!sym(input$selectintvar)) %>%
      left_join(probs() %>% select(!!sym(input$selectintvar), prob)) %>%
      left_join(probs() %>% select(name, prob), by = c("name")) %>%
      mutate(lift = value/(prob.x * prob.y),
             lift = if_else(!!sym(input$selectintvar) == name, NA_real_, lift),
             lift = round(lift, 2)) %>%
      select(!!sym(input$selectintvar), name, lift)
    })

    output$plot2 <- renderPlot({
      input$calc

    isolate(step2() %>%
      select(!!sym(input$selectintvar), unique(step1a() %>% pull(!!sym(input$selectintvar)))) %>%
      mutate_if(is_integer, ~. / max(.)) %>%
      pivot_longer(-!!sym(input$selectintvar)) %>%
      mutate(value = if_else(!!sym(input$selectintvar) == name, NA_real_, value)) %>%
      left_join(lift()) %>%
      ggplot(aes(name, !!sym(input$selectintvar))) +
      geom_tile(aes(fill= lift)) +
      labs(title = "What is the Lift for a Product if Another Product is Purchased?",
           x = "Given this was Purchased",
           y = "What is the Lift for this Product?") +
      # scale_fill_continuous(labels = scales::percent_format()) +
      geom_text(data = labs_text(), aes(label = value), size = 7) +
      geom_text(aes(label = round(lift,2)), color = "white", size = 7) +
      theme_bw() +
      theme(legend.title = element_blank(),
            text = element_text(size = 20),
            axis.text.x = element_text(angle = 90, vjust = .5, hjust = .95)))
  })

  #Downloading data
  #Chart data
  chartdata <- reactive({
    step2() %>%
      select(!!sym(input$selectintvar), unique(step1a() %>% pull(!!sym(input$selectintvar)))) %>%
      mutate_if(is_integer, ~. / max(.)) %>%
      pivot_longer(-!!sym(input$selectintvar)) %>%
      rename(pct = value) %>%
      left_join(labs_text()) %>%
      rename(GivenThis = 2, WhatPctOfThis = 1)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ChartData", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(chartdata(), file, row.names = FALSE)
    }
  )

  #step2a data
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("ObsData", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(step2a() %>% rename(GivenThis = !!sym(input$selectintvar)), file, row.names = FALSE)
    }
  )

  #Dynamic changes to inputs
  observeEvent(data(),{
    updatePickerInput(session, "selectvar",
                      choices = names(data()),
                      selected = names(data())[[1]]
    )})
  observeEvent(data(),{
    updatePickerInput(session, "selecttime",
                      choices = unique(data()$`Fiscal Qtr`),
                      selected = unique(data()$`Fiscal Qtr`)
    )})
  observeEvent(data(),{
    updatePickerInput(session, "selectobs",
                      choices = names(data()),
                      selected = names(data())[[1]]
    )})
  observeEvent(data(),{
    updatePickerInput(session, "selectintvar",
                      choices = names(data()),
                      selected = names(data())[[2]]
    )})
  observeEvent(input$selectintvar,{
    updatePickerInput(session, "selectvarvals",
                      choices = unique(step1() %>% pull(!!sym(input$selectintvar))),
                      selected = unique(step1() %>% pull(!!sym(input$selectintvar)))
    )})
  observeEvent(data_p(),{
    updatePickerInput(session, "selectp",
                      choices = data_p(),
                      selected = data_p()
    )})

}

shinyApp(ui = ui, server = server)
