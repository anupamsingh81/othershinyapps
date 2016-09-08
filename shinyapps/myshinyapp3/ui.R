library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Parrondo's Paradox"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("p", "p for each Game A:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.45, step = 0.01),
      sliderInput("p1", "p1 for each Game B:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.01, step = 0.01),
      sliderInput("p2", "p2 for each Game B:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.9, step = 0.01),
      sliderInput("gamma", "gamma for each Game C:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5, step = 0.01),
      sliderInput("SimLength", "Length of run:",
                  min = 100,
                  max = 5000,
                  value = 500, step = 100),
      sliderInput("MyInitialCapital", "Initial capital:",
                  min = 10,
                  max = 200,
                  value = 20, step = 5),
      sliderInput("replicates", "Number of replicates",
                  min=5, max = 100, step = 5, value=5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("SimGame")#,
      #plotOutput("SimGameB"),
      #plotOutput("SimGameC")
    )
  )
))
