


library(shiny)

shinyServer(function(input, output) {
  
  
    
    
  
  
  
  
  
  
  
  
  
  
  bayes <- reactive({
    
    
    A = c(input$r)
    B = c(input$u)
    C = c(input$v)
    Y = ifelse(A>20 & B> 20,"betablocker",ifelse(A>20&B<20,"ace",ifelse(A<20&C>40,"Diuretic","CCB")))
    Y })
  
  
  
  
  output$bayes.out <- renderPrint({
    bayes()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$parametriclog <- renderTable({
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    if (min(my.df[,1]) <= 0 ){return(NULL)}
    means <- tapply(log(my.df[,1]), my.df[,2], mean)
    sds <- tapply(log(my.df[,1]), my.df[,2], sd)
    ses<- tapply(log(my.df[,1]), my.df[,2], function(x) sd(x)/sqrt(length(x)))
    parametric <- data.frame(mean = c(means[1],means[2] ), sd=c(sds[1], sds[2]), se=c(ses[1], ses[2]))
    return(parametric)
  })
  
  
  
})
