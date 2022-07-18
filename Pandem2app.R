library(shiny)
library(vroom)
library(tidyverse)
library(Pandem2simulator)
options(shiny.maxRequestSize = 20 * 1024^2)

ui <- fluidPage(
  navbarPage(
    "Test",
    id="main_navbar",
    tabPanel(
      "1) Upload",
      fluidRow(

        fileInput("upAge", "testset", accept = c(".csv", ".tsv")),
        dataTableOutput("headAge")
      ),
      fluidRow(

        fileInput("upVariant", "trainset", accept = c(".csv", ".tsv")),
        dataTableOutput("headVariant")

      )
    ),
    tabPanel(
      "2) Visualisation",
      fluidRow(
        plotOutput("plot")

      ),
      fluidRow(
        plotOutput("plot2")

      )

    ),
    tabPanel(
      "3) KNN",
      fluidRow(
        selectInput("var","Colonne à ajouter", choices = character()),
        dataTableOutput("single")
      ),
      fluidRow(
        selectInput("class","Colonne", choices = character(),multiple = T),
        dataTableOutput("class")
      ),
      fluidRow(
        numericInput("factor", "factor", 500,
                     1, 10000000, 1)),
      fluidRow(
        actionButton("button", "Go simulate!"),
        downloadButton("download", "Download .csv"))

    )
  )
)
server <- function(input, output, session) {
  dataAge <- reactive({
    req(input$upAge)

    ext <- tools::file_ext(input$upAge$name)
    switch(ext,
           csv = vroom::vroom(input$upAge$datapath, delim = ","),
           tsv = vroom::vroom(input$upAge$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  dataVariant <- reactive({
    req(input$upVariant)

    ext <- tools::file_ext(input$upVariant$name)
    switch(ext,
           csv = vroom::vroom(input$upVariant$datapath, delim = ","),
           tsv = vroom::vroom(input$upVariant$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  output$headAge <- renderDataTable(dataAge(),options = list(pageLength = 5))
  output$headVariant <- renderDataTable(dataVariant(),options = list(pageLength = 5))
  observeEvent(dataVariant(), {
    updateSelectInput(session, "var", choices = names(dataVariant()))
  })
  output$single <- renderDataTable({dataVariant()[input$var]},options = list(pageLength = 5))
  observeEvent(dataAge(), {
    updateSelectInput(session, "class", choices = names(dataAge()))
  })
  output$class <- renderDataTable({dataAge()[input$class]},options = list(pageLength = 5))

  knn <- eventReactive(input$simulate, {
    simulator(trainset =  dataVariant(),testset = dataAge(),outcome = "variant",time = "time",geolocalisation = "country",factor = 500,count = "new_cases")
  })

  output$knn <- renderDataTable(knn(),options = list(pageLength = 5))


   output$download <- downloadHandler(
    filename = function() {
      paste0("knn", ".csv")
    },
    content = function(file) {
      vroom::vroom_write(knn(), file)
    }
  )




  uniq_variant <- reactive({
    uniq_variant <- unique(dataVariant()$variant)
  myColors <- rainbow(length(uniq_variant))
  names(myColors) <- uniq_variant
  myColors[names(myColors)=="NSQ"] = "#606060"
  colScale <- scale_fill_manual(name = "variant",values = myColors)
  })
  output$plot <- renderPlot({
    ggplot(data=dataVariant(), aes(x=time, y=new_cases, fill=variant)) +
      geom_bar(stat="identity") + ggtitle("BE ecdc") + theme(axis.text.x = element_text(angle = 90))+ uniq_variant()

  }, res = 96)
  output$plot2 <- renderPlot({
    ggplot(data=dataAge(), aes(x=time, y=new_cases)) +
      geom_bar(stat="identity") + ggtitle("Belgium")+ xlab("Group age") + ylab("number")+facet_wrap(vars(age_group))+ theme(axis.text.x = element_text(angle = 90))

  }, res = 96)

}
shinyApp(ui, server)
