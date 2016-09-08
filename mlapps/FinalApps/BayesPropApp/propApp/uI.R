shinyUI(fluidPage(
  titlePanel("Bayesian prop .test"),
  sidebarLayout(
    sidebarPanel(
      
      # Copy the line below to make a number input box into the UI.
      numericInput("a", label = h3("No.of previous success in controls"), value = 1),
      
      numericInput("b", label = h3("No.of previous failures in Controls"), value = 1),
      
      
      numericInput("c", label = h3("No.of previous success in Test"), value = 1),
      
      numericInput("d", label = h3("No.of previous failures in Test"), value = 1),
      
      numericInput("y1", label = h3("No.of current success in Test"), value = 50),
      
      numericInput("n1", label = h3("Total number of Events in Test"), value = 100),
      
      numericInput("y2", label = h3("No.of current success in controls"), value = 10),
      
      numericInput("n2", label = h3("Total number of Events in controls"), value = 100)
      
    ),
    
    mainPanel(
    
      h3("Basic statistics"), 
      verbatimTextOutput("summary.out") ,
      br(),
      
      h3("PlotOfDifference"),
      plotOutput("plot.out")
      
      
      
    )
    
  )
)
)   

                        