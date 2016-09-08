

library(shiny)

shinyUI(fluidPage(
  titlePanel("Bayesian t-test"),
  sidebarLayout(
    sidebarPanel(
      
      
      numericInput("v", label = h3("CP"), value = 60),
      
      numericInput("u", label = h3("BP"), value = 42),
      
      
      numericInput("r", label = h3("AGE"), value = 17)
      
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data loading",  
                 p("This displays a table of your data (to check for errors)")
                 ),
        tabPanel("Data checking",
                 h1(" display of a comparative probability distribution of Mean Difference with credible intervals"),
                 
                 
                 h2(" display of a comparative probability distribution of our Effect Size with credible intervals"),
                 
                 
                 tabPanel("Parametric inference",
                          h3("Conditional"),
                          verbatimTextOutput("bayes.out"),
                    
                          br(),
                          
                          
                          
                          p("Thus we successfully performed Bayesian t test with help of packages from Rasmus Baath and John Kruschke"),
                          textOutput('pvaluelog')))
      )
      
      
    ))))
