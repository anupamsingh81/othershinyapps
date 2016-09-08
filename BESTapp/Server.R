library(shiny)
library(BEST)

shinyServer(function(input, output) {


summary<- reactive ({
  x <- input$data1
  x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
  x <- x[!is.na(x)]
  
  y <- input$data2
  y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
  y <- y[!is.na(y)]
  
  BESTout = BESTmcmc(x,y,numSavedSteps = 20000)
  summary(BESTout)
  })

output$summary.out <- renderPrint({
  summary()
})

})