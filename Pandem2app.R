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

        fileInput("upAge", "testset", accept = c(".csv", ".tsv"))
      ),
      fluidRow(

        dataTableOutput("headAge"),
        plotOutput("plot2")
      ),
      fluidRow(

        fileInput("upVariant", "trainset", accept = c(".csv", ".tsv"))

      ),
      fluidRow(

        dataTableOutput("headVariant"),
        plotOutput("plot")
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
      fluidRow(column(6,
                      numericInput("factor", "factor", 500,
                                   1, 10000000, 1)),
               column(6,
                      selectInput("geolocalisation","Split by", choices = character())
               )),
      fluidRow(
        actionButton("button", "Go simulate!"),
        dataTableOutput("knn"),
        downloadButton("download", "Download .csv"))

    ),
    tabPanel(
      "4) Visualisation Knn",
      fluidRow(
        plotOutput("plotknn")

      )

    ),
    tabPanel(
      "5) enrichment",
      fluidRow(
        selectInput("variable","selectionner les variables", choices = "variant"),
        selectInput("group","selectionner les groups", choices = character()),
        selectInput("variant","selectionner le variant",choices = character()),
        numericInput("multiplicateur", "multiplicateur", 1.2,
                     1, 10000000, 0.1),
        actionButton("button2", "Go enrichment"),
        dataTableOutput("enrichment")
      )

    ),
    tabPanel(
      "6) Visualisation enrichment",
      fluidRow(
        plotOutput("plotenrichment")

      )

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

  observeEvent(dataAge(), {
    updateSelectInput(session, "geolocalisation", choices = names(dataAge()))
  })
  output$class <- renderDataTable({dataAge()[input$class]},options = list(pageLength = 5))

  dataknn <- eventReactive(input$button, {
    simulator(trainset = dataVariant(),testset = dataAge(),geolocalisation =input$geolocalisation,outcome = input$var,count = 'new_cases', time= input$class,factor = input$factor)

  })

  output$knn <- renderDataTable(dataknn(),options = list(pageLength = 5))


  output$download <- downloadHandler(
    filename = function() {
      paste0("knn", ".csv")
    },
    content = function(file) {
      vroom::vroom_write(dataknn(), file)
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

  output$plotknn <- renderPlot({
    ggplot(data=dataknn(), aes(x=time, y=new_cases, fill=variant)) +
      geom_bar(stat="identity") + ggtitle("BE prediction") + theme(axis.text.x = element_text(angle = 90))+
      uniq_variant() + facet_wrap(vars(age_group))
  }, res = 96)



  observeEvent(dataknn(), {
    updateSelectInput(session, "variable", choices = names(dataknn()))
  })
  observeEvent(dataknn(), {
    updateSelectInput(session, "group", choices = unlist(unique(dataknn()[input$variable])))
  })
  observeEvent(dataknn(), {
    updateSelectInput(session, "variant", choices = names(dataknn()))
  })
  enrichment <- eventReactive(input$button2,{
    enrichment_variant(data_aggregated = dataknn(),
                       variable = c("age_group"),group = c("<15yr"),
                       variants = "B.1.1.7", multiplicateur = 1.2,time = "time")
  })

  output$enrichment <- renderDataTable(enrichment(),options = list(pageLength = 5))

  output$plotenrichment <- renderPlot({
    ggplot(data=enrichment(), aes(x=time, y=new_cases, fill=variant)) +
      geom_bar(stat="identity") + ggtitle("with enrichment") +
      theme(axis.text.x = element_text(angle = 90))+ xlab("Group") + ylab("number")+uniq_variant() + facet_wrap(vars(age_group))
  }, res = 96)

}
shinyApp(ui, server)
