library(tidyr)
library(dplyr)

add_variable <- function(data,nomVariable,pourcentage,group)
{
  for (i in 1:length(group)) {
    data = data %>% mutate(!!sym(group[i]) := round(cases*pourcentage[i]))
  }
  data = data %>% select(-cases) %>%pivot_longer(group, names_to = nomVariable, values_to = "cases")

  return(data)
}

NumberUI <- function(id){
  ns <- NS(id)
  textInput(ns("obs"), "group:")
}

PourcentageUI <- function(id){
  ns <- NS(id)
  sliderInput(ns("Poucentage"), "pourcentage", 0, 1, 0.01)
}


Group <- function(input, output, session){
  reactive(input$obs)
}

Pourcentage <- function(input, output, session){
  reactive(input$Poucentage)
}
ui <- fluidPage(
  fileInput("dataset", "data", accept = c(".csv", ".tsv")),
  textInput("nomVariable", "Names new variables", value = "",placeholder =  "vaccination"),
  numericInput("numInputs", "How many inputs do you want", 2),
  uiOutput("inputGroup"),
  uiOutput("inputpourcentage"),
  textOutput('final'),
  textOutput('final2'),
  actionButton("button", "Go add variable"),
  dataTableOutput("datatable")
)

server <- function(input, output, session) {



  datadownload <- reactive({
    req(input$dataset)

    ext <- tools::file_ext(input$dataset$name)
    switch(ext,
           csv = vroom::vroom(input$dataset$datapath, delim = ","),
           tsv = vroom::vroom(input$dataset$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })


  observeEvent(input$numInputs, {
    output$inputGroup = renderUI({
      lapply(1:input$numInputs, function(i) {
        NumberUI(i)
      })
    })
  })
  observeEvent(input$numInputs, {
    output$inputpourcentage = renderUI({
      lapply(1:input$numInputs, function(i) {
        PourcentageUI(i)
      })
    })
  })

  test <- reactive({
    c(unlist(lapply(1:input$numInputs, function(i) {
      callModule(Group, as.character(i))()
    })))
  })
  test2 <- reactive({
    c(unlist(lapply(1:input$numInputs, function(i) {
      callModule(Pourcentage, as.character(i))()
    })))
  })

  output$final <- renderPrint({
    test()
  })
  output$final2 <- renderPrint({
    test2()
  })
  dataend <- eventReactive(input$button, {
    if(!exists("dataset")){dataset <<- datadownload()}

    dataset <<- add_variable(data = dataset,nomVariable = input$nomVariable,pourcentage = test2(),group = test())


    })

  output$datatable <-  DT::renderDataTable(dataend(),editable = TRUE)

}


shinyApp(ui, server)
