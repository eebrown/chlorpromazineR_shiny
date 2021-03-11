library(shiny)
library(chlorpromazineR)
library(shinyjs)

ui <- fluidPage(

  useShinyjs(),

  titlePanel("Calculate Antipscyhotic Dose Equivalents with chlorpromazineR"),

  br(),

  "This calculator can be used to calculate antipsychotic dose equivalents.",
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
                                    "Leucht et al. 2016" = "leucht2016",
                                    "Woods et al. 2003" = "woods2003")),

           selectInput(inputId = "in_route", label = "Route",
                       choices = c("Oral" = "oral",
                                   "Long-acting injectable" = "lai",
                                   "Short-acting injectable" = "sai")),

           "Antipsychotics available for this reference and route include: ",
           textOutput("options"),

           br(),
           br(),

           textInput(inputId = "in_from", label = "Antipsychotic to convert from:", value = "haloperidol"),
           textInput(inputId = "in_dose", label = "Dose (mg):", value = 10),
           textInput(inputId = "in_q", label = "Frequency (days) for LAI:", value = 14),

           textInput(inputId = "in_to", label = "Antipsychotic to convert to:", value = "chlorpromazine"),
    ),

  ),

  br(),

  textOutput("result"),

  br(),


  br(),
  textOutput("version"),
  "Shiny App Author: Eric E. Brown",

)

server <- function(input, output) {

  hide("in_q")

  observeEvent(input$in_route, {
    if (as.character(input$in_route) == "lai") show("in_q")
    if (as.character(input$in_route) == "oral") hide("in_q")
    if (as.character(input$in_route) == "sai") hide("in_q")
  })


  output$result <- renderText({

    input_data <- data.frame(antipsychotic = as.character(input$in_from),
                             dose = as.numeric(input$in_dose),
                             q = as.numeric(input$in_q))

    if (input$in_reference == "gardner2010") key <- gardner2010
    if (input$in_reference == "davis1974") key <- davis1974
    if (input$in_reference == "leucht2016") key <- leucht2016
    if (input$in_reference == "woods2003") key <- woods2003

    result <- to_ap(input_data, ap_label = "antipsychotic",
                    convert_to_ap = input$in_to, dose_label = "dose",
                    route = input$in_route, q = "q", key = key)


    as.character(paste(input$in_dose, "mg of", input$in_from,
                       "is equivalent to", round(result$ap_eq, 1), " mg of", input$in_to))

    })

  output$options <- renderText({

    if (input$in_reference == "gardner2010") key <- gardner2010
    if (input$in_reference == "davis1974") key <- davis1974
    if (input$in_reference == "leucht2016") key <- leucht2016
    if (input$in_reference == "woods2003") key <- woods2003

    if (input$in_route == "oral") route <- 1
    if (input$in_route == "sai") route <- 2
    if (input$in_route == "lai") route <- 3

    paste0(names(key[[route]]), "; ")
  })

  output$version <- renderText({
    paste("Package version: ", as.character(packageVersion("chlorpromazineR")))})


}

shinyApp(ui = ui, server = server)
