library(shiny)
library(shinyjs)
library(vroom)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(Pandem2Application)
library(spsComps)
options(shiny.maxRequestSize = 20 * 1024^2)

###Functions

#For Random simulator
NumberUIRandom <- function(id){
  ns <- NS(id)
  textInput(ns("obs"), "Group")
}

PourcentageUI <- function(id){
  ns <- NS(id)
  sliderInput(ns("Pourcent"), "Pourcentage", 0, 1, 0.1)
}

GroupRandom <- function(input, output, session){
  reactive(input$obs)
}

Pourcentage <- function(input, output, session){
  reactive(input$Pourcent)
}

#For Enrichment
row_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, shiny::uiOutput(ns("EnrichVar"))),
    column(8, shiny::uiOutput(ns("EnrichGroup")))
  )
} 

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

EnrichVarModule <- function(input, output, session) { #row_server
  return_value <- reactive({input$addgplabel})
  return_value2 <- reactive({input$addvariableinput})
  col <- dataset %>% select(!c("time", "cases"))
  output[['EnrichVar']] <- renderUI({
    ns <- session$ns
    tags$div(id = environment(ns)[['namespace']],
      tagList(
        selectInput(ns("addvariableinput"), "Variable", choices = names(col)),
      )
    )
  })
  output[['EnrichGroup']] <- renderUI({
    ns <- session$ns
    tags$div(id = environment(ns)[['namespace']],
             tagList(
               fluidRow(
                 column(6, 
                        selectInput(ns("addgplabel"),"Group", choices = unique(dataset[, input$addvariableinput]))
                 ),br(),
                 column(6,
                        column(3, actionButton(ns('deleteButton'), '', icon = shiny::icon('times', verify_fa = FALSE) , class = "btn-warning")),
                        column(6, h5("Click twice", style="color:#ff5233 ; margin-top: 3px;")),
                 )
               )
             )
    )
  })
  list(return_value = return_value, return_value2 = return_value2)
}

### User Interface
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:32px !important;
                            padding-bottom:0 !important;
                            height: 90px;
                            font-size: 25px ! important;
                            text-align: center;
                            }
                           .navbar {min-height:35px !important;}')),
  ),
  navbarPage(
    theme = shinythemes::shinytheme("cerulean"),
    title = span("Multiparametric simulator", style = "color: black"),
    id="main_navbar",
    tabPanel(
      "1) Datasets",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h3("Upload data"),br(),
          fileInput("dataset", "The dataset to which we will add a metadata. The name of the column reporting the number of cases must be named 'cases'.", accept = c(".csv", ".tsv"))
        ),
        mainPanel(
          br(),
          dataTableOutput("viewdataset"),
        )
      )
    ),
    tabPanel(
      "2) Simulations",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h3("Simulation type"), br(),
          radioButtons("simulationtype", label = ("Which type of simulation do you want to use ?"), choices = c("Random simulation", "Data driven simulation"), selected = NULL)
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.simulationtype == 'Random simulation'",
            br(), h3("Random simulation"), br(),
            div (id = "randomsim",
                 textInput("nomVariable", "Name new variable", value = "", placeholder =  "vaccination"),
                 numericInput("numInputs", "How many inputs do you want ?", 1, min = 2),
                 fluidRow(
                   column(4,
                          br(),br(), br(),
                          uiOutput("inputGroupRandom")
                   ),
                   column(4,
                          h4("Sliders should sum to 1!"),
                          uiOutput("inputpourcentage"),
                   )
                 )),
            actionButton("buttonRandom", "Go add variable !", class = "btn-primary"),
            disabled(actionButton("buttonremove", "Go remove !", class = "btn-warning")),
            br(), br(), br(),
            dataTableOutput("datatable"),
            uiOutput("downloadRan")
          ),
          conditionalPanel(
            condition = "input.simulationtype == 'Data driven simulation'",
            tabsetPanel(
              tabPanel(
                h3("Add data"), br(),
                div( id= "datadrivensim", 
                     fileInput("addData", "Upload new dataset. The name of the column reporting the number of cases must be named 'cases'.", accept = c(".csv", ".tsv")),
                     selectInput("var","Select one column for outcome", choices = character()),
                     selectInput("class","Select the names of the columns to match the data", choices = character(), multiple = T), 
                ),
                br(),
                actionButton("simulatebtn", "Go simulate!", class = "btn-primary"), 
                disabled(actionButton("buttonremovedriven", "Go remove !", class = "btn-warning")),
                br(), br(), br(),
                dataTableOutput("dataknn"),
                uiOutput("downloadSim")
              ),
              tabPanel(
                h3("Advanced parameters", style = "color:lightblue"), br(),
                radioButtons("split", label = "Split by", choices = c("Yes", "No"), selected = "No"),
                conditionalPanel(
                  condition = "input.split == 'Yes'",
                  selectInput("geolocalisation","Which column ?", choices = character(), selected = 1), 
                ),
                numericInput("factor", "Sequence number used by time for the trainset to reduce the execution time (factor)", 500, 1, 10000000, 1)
              )
            )
          )
        )
      )
    ),
    tabPanel(
      HTML("3) Visualization <br/> datasets"), 
      sidebarLayout(
        sidebarPanel(
          h3("Graph display of parameters"), br(),
          selectInput("colordata","Select the variable to be displayed in different colors (eg. variant)", choices = character()),
          selectInput("paneldata","Select the variable to be displayed in panel (eg. age_group)", choices = character()),
          radioButtons("Qfilterdata", label = "Do you want to filter with a third variable?", choices = c("Yes", "No"), selected = "No"),
          conditionalPanel(
            condition = "input.Qfilterdata == 'Yes'",
            h5("Which one ?", style="color:#555658; font-weight:bold"),
            fluidRow(
              column(4,
                     selectInput("filterdata","Variable", choices = character()),
              ),
              column(4,
                     uiOutput('filtergroupdata')
              )
            ),
          ),
          br(),
          actionButton("addgraphdatabtn", "Display graph", class = "btn-primary")
        ),
        mainPanel(
          br(),
          plotOutput("plot1")
        )
      )
    ),
    tabPanel(
      "4) Enrichment",
      sidebarLayout(
        sidebarPanel(
          h3("Parameters"), br(),
          h5("Which level of which variable (eg. variant) and group (eg. B.1.1.7) do you want to enrich ?", style="color:#555658; font-weight:bold"),
          fluidRow(
            column(4,
                   selectInput("variant","Variable ", choices = character()),
            ),
            column(4,
                   uiOutput('vargroup'),
            ),
          ),
          h5("In which variable(s) and group(s) do you want to apply the enrichment (eg. age_group <15y) ?", style="color:#555658; font-weight:bold"), 
          fluidRow(
            column(4,
                   selectInput("variable1", "Variable", choices = character()),
            ),
            column(4,
                   uiOutput('variablegroup'),
            ),
          ),
          actionButton("addvariablebtn", HTML("Add new variable <br/> and group"), class = "btn-info", style="margin-left: 355px;"),
          br(),
          actionButton("relativeriskbtn", "Display Relative Risk", class = "btn-primary"),
          br(),
          uiOutput("RR")
        ),
        mainPanel(
          br(),
          dataTableOutput("enrichment"),
          br(),
          uiOutput("downloadEnrich")
        )
      )
    ),
    tabPanel(
      HTML("5) Visualization <br/> enrichment"),
      sidebarLayout(
        sidebarPanel(
          h3("Graph display of parameters"), br(),
          selectInput("color","Select the variable to be displayed in different colors (eg. variant)", choices = character()),
          selectInput("panel","Select the variable to be displayed in panel (eg. age_group)", choices = character()),
          radioButtons("Qfilter", label = "Do you want to filter with a third variable?", choices = c("Yes", "No"), selected = "No"),
          conditionalPanel(
            condition = "input.Qfilter == 'Yes'",
            h5("Which one ?", style="color:#555658; font-weight:bold"),
            fluidRow(
              column(4,
                     selectInput("filter","Variable", choices = character()),
              ),
              column(4,
                     uiOutput('filtergroup')
              )
            ),
          ),
          br(),
          actionButton("addgraphbtn", "Display graphs", class = "btn-primary")
        ),
        mainPanel(
          br(),
          plotOutput("plots"),
          plotOutput("plotenrich")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ###Upload data
  datadownload <- reactive({
    req(input$dataset)
    ext <- tools::file_ext(input$dataset$name)
    switch(ext,
           csv = vroom::vroom(input$dataset$datapath, delim = ","),
           tsv = vroom::vroom(input$dataset$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$viewdataset <- DT::renderDataTable(datadownload())
  
  #Random simulator
  observeEvent(input$numInputs, {
    output$inputGroupRandom = renderUI({
      lapply(1:input$numInputs, function(i) {
        NumberUIRandom(i)
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
    c(unlist(lapply(0:input$numInputs, function(i) {
      callModule(GroupRandom, as.character(i))()
    })))
  })
  test2 <- reactive({
    c(unlist(lapply(0:input$numInputs, function(i) {
      callModule(Pourcentage, as.character(i))()
    })))
  })
  
  observeEvent(input$simulationtype == 'Data driven simulation', {
    reset("randomsim")
  })

  choice <<- "NULL"
  observeEvent(input$buttonRandom,{choice <<- "addrando"})
  observeEvent(input$simulatebtn,{choice <<- "addsimu"})
  observeEvent(input$buttonremove,{choice <<- "remove"})
  observeEvent(input$buttonremovedriven,{choice <<- "remove"})
  removedataset <<- c()
  
  dataend <- eventReactive(input$buttonRandom| input$buttonremove | input$simulatebtn | input$buttonremovedriven,{
    #print(choice)
    if(choice == "addrando"){
      validate(
        need(datadownload(), "Warning Upload data set."))
      validate(
        need(sum(test2())==1, "The sum of the sliders must be equal to 1."))
      req(sum(test2())==1)
      removedataset <<- c(gsub(" ", "_", input$nomVariable), removedataset)
      spsComps::shinyCatch({ dataset <<- add_variable(data = dataset, nomVariable = gsub(" ", "_", input$nomVariable), pourcentage = test2(), group = test()) })
      reset("randomsim")
      if (!rlang::is_empty(removedataset)) {
        enable("buttonremove")
        enable("buttonremovedriven")
      }
      else {
        disable("buttonremove")
        disable("buttonremovedriven")
      }
      return(dataset)
    }else if (choice == "addsimu"){
      removedataset <<- c(input$var, removedataset)
      spsComps::shinyCatch({
        if(input$split == "Yes"){
          dataset <<- simulator(trainset = dataAdd(), testset = dataset, geolocalisation = input$geolocalisation, time = input$class, outcome=input$var, count = "cases", factor= input$factor)
        }
        if(input$split == "No"){
          dataset <<- simulator_withoutsplit(trainset = dataAdd(), testset = dataset, time = input$class, outcome=input$var, count = "cases", factor= input$factor)
        }
      })
      reset("datadrivensim")
      reset("geolocalisation")
      if (!rlang::is_empty(removedataset)) {
        enable("buttonremove")
        enable("buttonremovedriven")
      }
      else {
        disable("buttonremove")
        disable("buttonremovedriven")
      }
      return(dataset)
    }else if (choice == "remove"){
      dataset <<- dataset %>% select(-removedataset[1])
      lnom <- length(colnames(dataset))-1
      dataset <<- dataset %>% group_by(across({colnames(dataset)[1:lnom]})) %>% summarise(cases = sum(cases))
      removedataset <<- removedataset[-1]
      if (!rlang::is_empty(removedataset)) {
        enable("buttonremove")
        enable("buttonremovedriven")
      }
      else {
        disable("buttonremove")
        disable("buttonremovedriven")
      }
      return(dataset)
    }
  })
  
  output$datatable <-{DT::renderDataTable(dataend(), editable = TRUE)} 
  
  output$downloadRan <- renderUI({
    req(input$buttonRandom, dataend())
    downloadButton("downloadrandom", "Download .csv", class = "btn-success")
  })
  
  output$downloadrandom <- downloadHandler(
    filename = function() {
      paste0("random-simulator", ".csv")
    },
    content = function(file) {
      write.csv(dataend(), file, row.names=FALSE)
    }
  )
  
  
  #Data driven simulator
  dataAdd <- reactive({
    req(input$addData)
    ext <- tools::file_ext(input$addData$name)
    switch(ext,
           csv = vroom::vroom(input$addData$datapath, delim = ","),
           tsv = vroom::vroom(input$addData$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  observeEvent(input$simulationtype == 'Random simulation', {
    reset("datadrivensim")
    reset("geolocalisation")
  })
  
  observeEvent(dataAdd(), {
    updateSelectInput(session, "var", choices = names(dataAdd()))
    updateSelectInput(session, "class", choices = names(dataAdd()))
    updateSelectInput(session, "geolocalisation", choices = names(dataAdd()))
  })
  
  output$dataknn <- renderDataTable(dataend(), options = list(pageLength = 10, scrollX=TRUE))

  output$downloadSim <- renderUI({
    req(input$simulatebtn, dataend())
    downloadButton("downloadsimulate", "Download .csv", class = "btn-success")
  })
  
  output$downloadsimulate <- downloadHandler(
    filename = function() {
      paste0("data-driven-simulator", ".csv")
    },
    content = function(file) {
      write.csv(dataend(), file, row.names=FALSE)
    }
  )
  
  ### Visualization datasets
  
  #Update params after download dataset
  observeEvent(input$dataset, {
    validate(
      need(input$dataset$datapath, "Warning Upload data set2"))
    dataset <<- datadownload()
    col <- dataset %>% select(!c("time", "cases"))
    updateSelectInput(session, "colordata", choices = names(col))
    updateSelectInput(session, "paneldata", choices = names(col))
    updateSelectInput(session, "filterdata", choices = names(col))
  })
  
  #Update params after random simulator
  observeEvent(dataend(), {
    col <- dataset %>% ungroup() %>% select(!c("time", "cases"))
    updateSelectInput(session, "colordata", choices = names(col))
    updateSelectInput(session, "paneldata", choices = names(col))
    updateSelectInput(session, "filterdata", choices = names(col))
  })
  
  output$filtergroupdata <- renderUI({
    selectInput("filtergroupdata","Group", choices = unique(dataset[, input$filterdata]))
  })
  
  uniq_variant <- reactive({ 
    uniq_variant <- unique(dataset$variant) 
    myColors <- rainbow(length(uniq_variant)) 
    names(myColors) <- uniq_variant
    myColors[names(myColors)=="NSQ"] = "#606060"
    colScale <- scale_fill_manual(name = "variant", values = myColors)
  })
  
  output$plot1 <- renderPlot({
    req(input$addgraphdatabtn, input$dataset)
    if (input$Qfilterdata=="Yes") {
      res <- filter(dataset, get(input$filterdata) == input$filtergroupdata)
      if (input$colordata=="variant") {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$colordata)) + ggtitle(paste("Prediction", input$colordata, input$paneldata, input$filterdata, sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$paneldata)
      }
      else {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$colordata)) + ggtitle(paste("Prediction", input$colordata, input$paneldata, input$filterdata, sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$paneldata)
      }
    }
    else {
      if (input$colordata=="variant") {
        ggplot(data=dataset, aes_string(x="time", y="cases", fill=input$colordata)) + ggtitle(paste("Prediction", input$colordata, input$paneldata, sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$paneldata)
      }
      else {
        ggplot(data=dataset, aes_string(x="time", y="cases", fill=input$colordata)) + ggtitle(paste("Prediction", input$colordata, input$paneldata, sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$paneldata)
      }
    }    
  }, res = 96)
  
  
  ### Enrichment
  
  #Update params after download dataset
  observeEvent(input$dataset, {
    dataset <<- datadownload()
    col <- dataset %>% select(!c("time", "cases"))
    updateSelectInput(session, "variable1", choices = names(col))
    updateSelectInput(session, "variant", choices = names(col))
  })
  
  observeEvent(dataend(), {
    col <- dataset %>% ungroup() %>% select(!c("time", "cases"))
    updateSelectInput(session, "variable1", choices = names(col))
    updateSelectInput(session, "variant", choices = names(col))
  })
  
  output$vargroup <- renderUI({
    selectInput("groupvar","Group", choices = unique(dataset[, input$variant]))
  })
  
  output$variablegroup <- renderUI({
    selectInput("gp","Group", choices = unique(dataset[, input$variable1]))
  })
  
  counter <- reactiveVal(0)
  observeEvent(input$addvariablebtn, counter(counter() + 1))
  
  rr <- eventReactive(input$relativeriskbtn,{
    #print(counter)
    if(counter() > 0) {
      params <- lapply(handler(), function(handle) {handle()})
      df <- data.frame(matrix(unlist(params), nrow=length(params), byrow=TRUE))
      row_odd <- seq_len(nrow(df)) %% 2
      data_row_odd <- df[row_odd == 1, ]
      data_row_even <- df[row_odd == 0, ]
      relatif_risk(data_aggregated = dataset,
                   variable = c(input$variable1, data_row_even), group = c(input$gp, data_row_odd),
                   col_enrichment = input$variant, group_enrichment=input$groupvar)
    }
    else {
      relatif_risk(data_aggregated = dataset,
                   variable = input$variable1, group = input$gp,
                   col_enrichment = input$variant, group_enrichment=input$groupvar) 
    }
  })
  
  output$RR <- renderUI({
    req(input$relativeriskbtn)
    div(
      br(),
      numericInput("multiplicateur", "Relative risk", round(rr(), digits = 2), 0, 1000, 0.01),
      actionButton("enrichmentbtn", "Go enrichment!", class = "btn-primary")
    )
  })
  
  handler <- reactiveVal(list())
  observeEvent(input$addvariablebtn, {
    i <- sprintf('%04d', input$addvariablebtn)
    id <- sprintf('EnrichGroup%s', i)
    insertUI(
      selector = '#addvariablebtn',
      where = "beforeBegin",
      ui= row_ui(id)
    )
    print("ok")
    handler_list <- isolate(handler())
    new_handler <- callModule(EnrichVarModule, id)
    print("ok1")
    handler_list <- c(handler_list, new_handler)
    names(handler_list)[length(handler_list)] <- id
    handler(handler_list)
    print("ok2")
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      j <- as.integer(input[[paste0(id, '-deleteButton')]])
      if((j %% 2) == 0) {
        counter(counter()-1)
        if (counter() == 0) {
          handler <- NULL
        }
      }
      removeUI(selector = sprintf('#%s', id), multiple=TRUE)
      remove_shiny_inputs(id, input)
      
    })
    print("ok3")
  })
  
  enrichment <- eventReactive(input$enrichmentbtn,{
    req(input$relativeriskbtn)
    if(counter() > 0) {
      params <- lapply(handler(), function(handle) {handle()})
      df <- data.frame(matrix(unlist(params), nrow=length(unlist(params)), byrow=TRUE))
      row_odd <- seq_len(nrow(df)) %% 2
      data_row_odd <- na.omit(df[row_odd == 1, ])
      data_row_even <- na.omit(df[row_odd == 0, ])
      spsComps::shinyCatch({
        enrichment_variant(data_aggregated = dataset,
                           variable = c(input$variable1, data_row_even), group = c(input$gp, data_row_odd),
                           col_enrichment = input$variant, group_enrichment=input$groupvar,
                           multiplicateur = input$multiplicateur, time ="time")
      },
      blocking_level = "error",
      prefix = "Enrichment"
      )
    }
    else {
      spsComps::shinyCatch({
        enrichment_variant(data_aggregated = dataset,
                           variable = input$variable1, group = input$gp,
                           col_enrichment = input$variant, group_enrichment=input$groupvar,
                           multiplicateur = input$multiplicateur, time ="time")
      },
      blocking_level = "error",
      prefix = "Enrichment" 
      )
    }
  })
  
  output$enrichment <- renderDataTable(enrichment(), options = list(pageLength = 10))
  
  output$downloadEnrich <- renderUI({
    req(input$enrichmentbtn, enrichment())
    downloadButton("downloadenrichment", "Download .csv", class = "btn-success")
  })
  
  output$downloadenrichment <- downloadHandler(
    filename = function() {
      paste0("enrichment", ".csv")
    },
    content = function(file) {
      write.csv(enrichment(), file, row.names=FALSE)
    }
  )
  
  
  ### Visualization Enrichment
  observeEvent(input$enrichmentbtn, {
    col <- dataset %>% ungroup() %>% select(!c("time", "cases"))
    updateSelectInput(session, "color", choices = names(col))
    updateSelectInput(session, "panel", choices = names(col))
    updateSelectInput(session, "filter", choices = names(col))
  })
  
  output$filtergroup <- renderUI({
    selectInput("filtergroups", "Group", choices = unique(dataset[, input$filter]))
  })
  
  output$plots <- renderPlot({
    req(input$addgraphbtn, input$dataset)
    if (input$Qfilter=="Yes") {
      res <- filter(dataset, get(input$filter) == input$filtergroups)
      if (input$color=="variant") {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, input$filter, input$filtergroups, "without enrichment", sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
      }
      else {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, input$filter, input$filtergroups, "without enrichment", sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$panel)
      }
    }
    else {
      if (input$color=="variant") {
        ggplot(data=dataset, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, "without enrichment", sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
      }
      else {
        ggplot(data=dataset, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, "without enrichment", sep=" ")) +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$panel)
      }
    }    
  }, res = 96)
  
  output$plotenrich <- renderPlot({ 
    req(input$addgraphbtn, enrichment())
    if (input$Qfilter=="Yes") {
      res <- filter(enrichment(), get(input$filter) == input$filtergroups)
      if (input$color=="variant") {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
      }
      else {
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$panel)
      }
    }
    else {
      if (input$color=="variant") {
        ggplot(data=enrichment(), aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
      }
      else {
        ggplot(data=enrichment(), aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$panel)
      }
    }    
  }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
