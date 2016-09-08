library(shiny)

shinyUI(fluidPage(
  titlePanel("Prognosis of ICH by machine learning and conventional scores"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      selectInput(
        "logs", "Log transform",
        c(None = "no",
          Logs = "yes")),
      
      numericInput("a", label = h3("Age"), value = 85), 
      
      
      
      numericInput("b", label = h3("sex"), value = 1), 
      
      
      
      
      
      numericInput("c", label = h3("MAP"), value = 160 ), 
      
      
      
      numericInput("d", label = h3("GCS"), value = 6) ,
      
      
      
      numericInput("e", label = h3("RBS"), value = 230),
      
      
      
      numericInput("f", label = h3("tentorium"), value = 1),
      
      
      
      numericInput("g", label = h3("volume"), value = 1), 
      
      
      
      numericInput("h", label = h3("VE"), value = 1),  
      
      
      
      numericInput("i", label = h3("MidlineShift"), value = 1)
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data loading",  
                 h1("This displays a table of your data (to check for errors)"),
                 tableOutput('contents')),
        tabPanel("Data checking",
                 h1("This displays plot of descriptive data"),
                 plotOutput('plot'),
                 
                 tabPanel("Parametric inference",
                          h2("Key summary statistics"),
                          verbatimTextOutput("k.out"),
                          
                          br(),
                          h3("Details on the ICH Score"),
                          verbatimTextOutput("roc.out"),
                          br(),
                          
                          
                          
                          h3("Details on the Logistic Regression Algorithm"), 
                          
                          verbatimTextOutput("lr.out"), 
                          
                          br(),
                          
                          
                          
                          
                          
                          
                          p("Thus we successfully performed ICH Prognostication.This Web App has been developed by Anupam Kumar Singh,MD"),
                          textOutput('pvaluelog')))
      )
      
      
    ))))

