library("ggplot2")
library(BayesFactor)
library(shiny)

shinyServer(function(input, output) {
  
  
  
  
  
  
  
  
  
  
 
  t <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
   
    score <- c(x, y)
    group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
    
    normal.t <- t.test(score ~ group, var.equal=TRUE)
    Welch.t <- t.test(score ~ group, var.equal=FALSE)
    newdf = data.frame(group,score)
    g = ggplot(newdf,aes(group,score))+ geom_bar(stat= "identity")
    bf = ttestBF(formula= score ~ group,data=newdf )
    p = list(normal.t,Welch.t)
    list(p,g,bf)
  })
  
  output$t.out <- renderPrint({
    return(list(t()[1]))
  })
  
  
  output$bayes.out <- renderPrint({
    t()[3]
  })
  
  output$plot <- renderPlot({t()[2]})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
