

library(shiny)

shinyUI(fluidPage(
  titlePanel("BayesianTwo sample t-test"),
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
          Logs = "yes"))
     
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Data loading",  
       h1("This displays a table of your data (to check for errors)"),
       tableOutput('contents')),
      tabPanel("Data checking",
       h1("This displays comparative Bayesian plot of your data"),
       plotOutput('plot'),
       
        ),
      tabPanel("Parametric inference",
       h2("Key summary statistics"),
       
       textOutput('summary')))
      )
      
      
      ))))

