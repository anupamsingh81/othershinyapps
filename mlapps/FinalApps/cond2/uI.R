shinyUI(fluidPage(
  titlePanel("ALGORITHM BP"),
  
  sidebarLayout(
    sidebarPanel(  numericInput("v", label = h3("CP"), value = 60),
                   
                   numericInput("u", label = h3("BP"), value = 42),
                   
                   
                   numericInput("r", label = h3("AGE"), value = 17)
    ),
    mainPanel(
      h1("Drug to be used in the patient"),
      verbatimTextOutput("bayes.out"))
      
  )
))