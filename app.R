library(shiny)
library(chlorpromazineR)
library(shinyjs)

ui <- fluidPage(

  useShinyjs(),

  titlePanel("Calculate Antipscyhotic Dose Equivalents with chlorpromazineR"),

  br(),  br(),

  "This calculator can be used to calculate antipsychotic dose equivalents based
  on the reported literature (see references below).",

  br(),  br(),

  strong("*This is beta software and may contain errors. It is not meant to be
  used to guide clinical decisions. Interpretation of the conversion depends on
  understanding the methods and limitations of the underlying research (see
         references below)."),

  br(),  br(),

  "For bulk calculation or research use, the R package ",
  tags$a(href = "https://cran.r-project.org/package=chlorpromazineR",
         "chlorpromazineR"),
  " is more appropriate. Please see the ",
  tags$a(href = "https://docs.ropensci.org/chlorpromazineR/articles/walkthrough.html",
         "documentation"),
  "for data sources and references, limitations and other important details.",

  br(),
  br(),
  fluidRow(
    column(6,

           selectInput(inputId = "in_reference", label = "Reference",
                       choices = c("Davis et al. 1974" = "davis1974",
                                    "Gardner et al. 2010" = "gardner2010",
                                    "Leucht et al. 2020" = "leucht2020",
                                    "Leucht et al. 2016" = "leucht2016",
                                    "Woods et al. 2003" = "woods2003")),

           selectInput(inputId = "in_route", label = "Route",
                       choices = c("Oral" = "oral",
                                   "Long-acting injectable" = "lai")),

           selectInput(inputId = "select_from",
                       label = "Antipsychotic to convert from:",
                       choices = NULL),

           textInput(inputId = "in_dose", label = "Dose (mg):", value = 10),

           textInput(inputId = "in_q", label = "Frequency (days) for LAI:",
                     value = 14),

           selectInput(inputId = "in_to",
                       label = "Antipsychotic to convert to:", choices = NULL)
      ),
    ),

  br(),
  h3(textOutput("result")),
  br(),
  textOutput("version"), "Shiny App Author: Eric E. Brown",
  br(), br(),
  strong("References:"),
  br(),
  br(), "Brown, Eric, Parita Shah, and Julia Kim. ChlorpromazineR: Convert Antipsychotic Doses to Chlorpromazine Equivalents. (2019). Comprehensive R Archive Network. <https://doi.org/10.5281/zenodo.3483139>",
  br(), "Davis, J. (1974). Dose equivalence of the anti-psychotic drugs. Journal of Psychiatric Research, 11, 65-69. <https://doi.org/10.1016/0022-3956(74)90071-5>",
  br(), "Gardner, D. M., Murphy, A. L., O’Donnell, H., Centorrino, F., & Baldessarini, R. J. (2010). International consensus study of antipsychotic dosing. The American Journal of Psychiatry, 167(6), 686–693. <https://doi.org/10.1176/appi.ajp.2009.09060802>",
  br(), "Leucht, S., Samara, M., Heres, S., & Davis, J. M. (2016). Dose Equivalents for Antipsychotic Drugs: The DDD Method. Schizophrenia Bulletin, 42(suppl_1), S90–S94. <https://doi.org/10.1093/schbul/sbv167>",
  br(), "Leucht, S., Crippa, A., Siafis, S., Patel, M., Orsini, N. & Davis, J. M. (2020). Dose-Response Meta-Analysis of Antipsychotic Drugs for Acute Schizophrenia. American Journal of Psychiatry. 117(4) <https://doi.org/10.1176/appi.ajp.2019.19010034>",
  br(), "Woods, S. (2003). Chlorpromazine Equivalent Doses for the Newer Atypical Antipsychotics. Journal of Clinical Psychiatry. 64(6). 663-667. <https://doi.org/10.4088/JCP.v64n0607>"

)

server <- function(input, output, session) {

  observe({

    key <- get(input$in_reference)

    if (as.character(input$in_route) == "oral") x <- names(key$oral)
    if (as.character(input$in_route) == "lai") x <- names(key$lai)

    updateSelectInput(session, "select_from",
                      label = "Antipsychotic to convert from:",
                      choices = x,
                      selected = tail(x, 1))

    updateSelectInput(session, "in_to",
                      label = "Antipsychotic to convert to:",
                      choices = names(key$oral),
                      selected = tail(names(key$oral), 1))
  })

  hide("in_q")

  observeEvent(input$in_route, {
    if (as.character(input$in_route) == "lai") show("in_q") else hide("in_q")
  })

  output$result <- renderText({

    input_data <- data.frame(antipsychotic = as.character(input$select_from),
                             dose = as.numeric(input$in_dose),
                             q = as.numeric(input$in_q))

    key <- get(input$in_reference)

    result <- to_ap(input_data, ap_label = "antipsychotic",
                    convert_to_ap = input$in_to, dose_label = "dose",
                    route = input$in_route, q = "q", key = key,
                    convert_to_route = "oral")

    if (input$in_route == "lai") route_text <- paste("IM q", input$in_q, "days")
    if (input$in_route == "oral") route_text <- "po daily"

    as.character(paste0(input$select_from, " ", input$in_dose, " mg ",
                        route_text, " is equivalent to ", input$in_to, " ",
                        round(result$ap_eq, 1), " mg po daily.*" ))

    })

  output$version <- renderText({
    paste("Package version: chlorpromazineR",
          as.character(packageVersion("chlorpromazineR")))})

}

shinyApp(ui = ui, server = server)
