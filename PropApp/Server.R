library(shiny)

shinyServer(function(input, output) {
  
  
  
  summary <- reactive ({
    I = 1000
    theta1 = rbeta(I, input$y1+input$a, (input$n1-input$y1)+input$b) 
    theta2 = rbeta(I, input$y2+input$c, (input$n2-input$y2)+input$d)
    diff = theta1-theta2 
    quantiles = quantile(diff,c(0.005,0.025,0.5,0.975,0.995))
    quantiles
  })
  
  
  plot <- reactive({
    I = 1000
    theta1 = rbeta(I, input$y1+input$a, (input$n1-input$y1)+input$b) 
    theta2 = rbeta(I, input$y2+input$c, (input$n2-input$y2)+input$d)
    diff = theta1-theta2 
    quantiles = quantile(diff,c(0.005,0.025,0.5,0.975,0.995))
    hist(diff,col ="blue",main="Distribution of Difference",
         xlab="Difference in proportion in two groups")
    
         
  })
  
  output$summary.out <- renderPrint({
    summary()
  })
  
  output$plot.out <- renderPlot({
    print(plot())
  })
  
 
  
})
  