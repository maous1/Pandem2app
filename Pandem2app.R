library(shiny)
library(shinyjs)
library(vroom)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(spsComps)
library(Pandem2Application)
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
  col <- dataset %>% select(!c("time", "cases"))
  fluidRow(
    column(4, 
           selectInput(ns("addvariableinput"), "Variable", choices = names(col)),
    ),
    column(4,
           uiOutput(ns("ui_placeholder"))
    )
  )
} 

row_server <- function(input, output, session) {
  return_value <- reactive({input$addgplabel})
  return_value2 <- reactive({input$addvariableinput})
  ns <- session$ns
  output$ui_placeholder <- renderUI({
    selectInput(ns("addgplabel"),"Group", choices = unique(dataset[, input$addvariableinput]))
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
                    fileInput("dataset", "The dataset to which we will add a metadata. The name of the column reporting the number of cases must be named 'cases'.", accept = c(".csv", ".tsv")),
                    h3("Simulation type"),br(),
                    radioButtons("simulationtype", label = ("Which type of simulation do you want to use ?"), choices = c("Random simulation", "Data driven simulation"), selected = NULL)
                ),
                mainPanel(
                    conditionalPanel(
                        condition = "input.simulationtype == 'Random simulation'",
                        br(), h3("Random simulation"), br(),
                        textInput("nomVariable", "Name new variable", value = "", placeholder =  "vaccination"),
                        numericInput("numInputs", "How many inputs do you want ?", 2),
                        fluidRow(
                            column(4,
                                   br(),br(), br(),
                                   uiOutput("inputGroupRandom")
                            ),
                            column(4,
                                   h4("Sliders should sum to 1!"),
                                   uiOutput("inputpourcentage"),
                            )
                        ),
                        actionButton("buttonRandom", "Go add variable !", class = "btn-primary"),
                        br(), br(), br(),
                        dataTableOutput("datatable"),
                        uiOutput("downloadRan")
                    ),
                    conditionalPanel(
                        condition = "input.simulationtype == 'Data driven simulation'",
                        tabsetPanel(
                          tabPanel(
                            h3("Add data"), br(),
                            fileInput("addData", "Upload new dataset. The name of the column reporting the number of cases must be named 'cases'.", accept = c(".csv", ".tsv")),
                            selectInput("var","Select one column for outcome", choices = character()),
                            selectInput("class","Select the names of the columns to match the data", choices = character(), multiple = T), #Colonne (time) les colommunes dont on veut une classification
                            br(),
                            actionButton("simulatebtn", "Go simulate!", class = "btn-primary"), 
                            br(), br(), br(),
                            dataTableOutput("knn"),
                            uiOutput("downloadSim")
                          ),
                           tabPanel(
                            h3("Advanced parameters", style = "color:lightblue"), br(),
                            radioButtons("split", label = "Split by", choices = c("Yes", "No"), selected = "No"),
                            conditionalPanel(
                              condition = "input.split == 'Yes'",
                              selectInput("geolocalisation","Which column ?", choices = character(), selected = 1), #Split by (geolocalisation)
                            ),
                            numericInput("factor", "Sequence number used by time for the trainset to reduce the execution time (factor)", 500, 1, 10000000, 1)
                           )
                        )
                    )
                )
            )
        ),
        tabPanel(
          HTML("2) Visualization <br/> datasets"), 
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
          "3) Enrichment",
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
              div(id="placeholder"),
              fluidRow(
                column(5,
                       
                ),
                column(2,
                       #actionButton("cancelvariablebtn", "Cancel", class = "btn-secondary"),
                ),
                column(3,
                       actionButton("addvariablebtn", HTML("Add new variable <br/> and group"), class = "btn-secondary"),
                ),
              ),
              actionButton("relativeriskbtn", "Display Relative Risk", class = "btn-info"),
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
            HTML("4) Visualization <br/> enrichment"),
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
        c(unlist(lapply(1:input$numInputs, function(i) {
            callModule(GroupRandom, as.character(i))()
        })))
    })
    test2 <- reactive({
        c(unlist(lapply(1:input$numInputs, function(i) {
            callModule(Pourcentage, as.character(i))()
        })))
    })
    
    observeEvent(test2(), {
        if(sum(test2())==1){
            show("buttonRandom")
        }
        else{
            hide("buttonRandom")
        }
    })
    
    dataend <- eventReactive(input$buttonRandom, {
        dataset <<- add_variable(data = dataset, nomVariable = input$nomVariable, pourcentage = test2(), group = test())
    })
    
    output$datatable <- DT::renderDataTable(dataend(), editable = TRUE)
    
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
    
    observeEvent(dataAdd(), {
      updateSelectInput(session, "var", choices = names(dataAdd()))
      updateSelectInput(session, "class", choices = names(dataAdd()))
      updateSelectInput(session, "geolocalisation", choices = names(dataAdd()))
    })
    
    #Go simulate
    dataknn <- eventReactive(input$simulatebtn, {
        if(input$split == "Yes"){
          dataset <<- simulator(trainset = dataAdd(), testset = dataset, geolocalisation = input$geolocalisation, time = input$class, outcome=input$var, count = "cases", factor= input$factor)
        }
        if(input$split == "No"){
          dataset <<- simulator_withoutsplit(trainset = dataAdd(), testset = dataset, time = input$class, outcome=input$var, count = "cases", factor= input$factor)
        }
    })
    
    output$knn <- renderDataTable(dataknn(), options = list(pageLength = 10, scrollX=TRUE))
    
    output$downloadSim <- renderUI({
      req(input$simulatebtn, dataknn())
      downloadButton("downloadsimulate", "Download .csv", class = "btn-success")
    })
    
    output$downloadsimulate <- downloadHandler(
      filename = function() {
        paste0("data-driven-simulator", ".csv")
      },
      content = function(file) {
        write.csv(dataknn(), file, row.names=FALSE)
      }
    )
    
    ### Visualization datasets
    
    #Update params after download dataset
    observeEvent(input$dataset, {
      dataset <<- datadownload()
      col <- dataset %>% select(!c("time", "cases"))
      updateSelectInput(session, "colordata", choices = names(col))
      updateSelectInput(session, "paneldata", choices = names(col))
      updateSelectInput(session, "filterdata", choices = names(col))
    })
    
    #Update params after random simulator
    observeEvent(input$buttonRandom, {
      col <- dataset %>% select(!c("time", "cases"))
      updateSelectInput(session, "colordata", choices = names(col))
      updateSelectInput(session, "paneldata", choices = names(col))
      updateSelectInput(session, "filterdata", choices = names(col))
    })
    
    #Update params after data driven simulator
    observeEvent(input$simulatebtn, {
      col <- dataset %>% select(!c("time", "cases"))
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
            geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$paneldata) #facet_grid(get(input$filter) ~ get(input$panel))
        }
        else {
          ggplot(data=res, aes_string(x="time", y="cases", fill=input$colordata)) + ggtitle(paste("Prediction", input$colordata, input$paneldata, input$filterdata, sep=" ")) +
            geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(input$paneldata) #facet_grid(get(input$filter) ~ get(input$panel))
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
    
    #Update params after random simulator
    observeEvent(input$buttonRandom, {
      col <- dataset %>% select(!c("time", "cases"))
      updateSelectInput(session, "variable1", choices = names(col))
      updateSelectInput(session, "variant", choices = names(col))
    })
    
    #Update params after data driven simulator
    observeEvent(input$simulatebtn, {
      col <- dataset %>% select(!c("time", "cases"))
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
      new_id <- paste("row", input$addvariablebtn, sep = "_")
      insertUI(
        selector = "#placeholder",
        where = "beforeBegin",
        ui = row_ui(new_id)
      )
      handler_list <- isolate(handler())
      new_handler <- callModule(row_server, new_id)
      handler_list <- c(handler_list, new_handler)
      names(handler_list)[length(handler_list)] <- new_id
      handler(handler_list)
    })
    
    enrichment <- eventReactive(input$enrichmentbtn,{
      req(input$relativeriskbtn)
      if(counter() > 0) {
        params <- lapply(handler(), function(handle) {handle()})
        df <- data.frame(matrix(unlist(params), nrow=length(params), byrow=TRUE))
        row_odd <- seq_len(nrow(df)) %% 2
        data_row_odd <- df[row_odd == 1, ]
        data_row_even <- df[row_odd == 0, ]
        spsComps::shinyCatch({ #Display warning in UI
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
        spsComps::shinyCatch({ #Display warning in UI
          enrichment_variant(data_aggregated = datadownload(),
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
      col <- dataset %>% select(!c("time", "cases"))
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
          ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, input$filtergroups, "without enrichment", sep=" ")) +
            geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
        }
        else {
          ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle(paste("Prediction", input$color, input$panel, input$filtergroups, "without enrichment", sep=" ")) +
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
        ggplot(data=res, aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel) #facet_wrap(vars(group))
      }
      else {
        ggplot(data=enrichment(), aes_string(x="time", y="cases", fill=input$color)) + ggtitle("With enrichment") +
          geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) + uniq_variant() + facet_wrap(input$panel)
      }
    }, res = 96)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
