

library(shiny)

shinyUI(fluidPage(
  titlePanel("Bayesian t-test"),
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
     
     numericInput("v", label = h3("credible interval"), value = 0.95),
     
     numericInput("u", label = h3("ComparatorValueMean"), value = 0),
     
     
     numericInput("r", label = h3("ROPEhigh"), value = -1),
     
     numericInput("s", label = h3("ROPElow"), value = 1)
     
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Data loading",  
       h1("This displays a table of your data (to check for errors)"),
       tableOutput('contents')),
      tabPanel("Data checking",
       h1(" display of a comparative probability distribution of Mean Difference with credible intervals"),
       plotOutput('plot'),
       
       h2(" display of a comparative probability distribution of our Effect Size with credible intervals"),
       plotOutput('plot2'),
       
      tabPanel("Parametric inference",
       h3("Key Bayesian statistics"),
       verbatimTextOutput("bayessummary.out"),
       
       br(),
       h3("Details on the t-test"),
       verbatimTextOutput("t.out"),
       br(),
       
      
       p("Thus we successfully performed Bayesian t test with help of packages from Rasmus Baath and John Kruschke"),
       textOutput('pvaluelog')))
      )
      
      
      ))))

