library("BEST")
library(BayesFactor)
library(shiny)

shinyServer(function(input, output) {
  
  
  
  
  
  
  
  
  output$plot <- renderPlot({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    z= c(input$r,input$s)
    a= c(input$t)
    d = c(input$u)
    e = c(input$v)
    BESTout<-BESTmcmc(x,y,numSavedSteps = 20000)
    plot(BESTout,which = "mean" ,credMass = e, ROPE =z, compVal = d) })
  
  
  
  
  bayes <- reactive({
    x <- input$data1
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    
    y <- input$data2
    y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
    y <- y[!is.na(y)]
    z= c(input$r,input$s)
    a= c(input$t)
    d = c(input$u)
    e = c(input$v)
    BESTout<-BESTmcmc(x,y,numSavedSteps = 20000)
    summary(BESTout,credMass= e, ROPEm= z, compValm =d ,compValeff=1,
            ROPEeff=c(-0.2,0.2)) 
  })
  
  
 
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
    bf = ttestBF(formula= score ~ group,data=newdf )
    
    return(list(normal.t, Welch.t,bf))
  })
  
  output$t.out <- renderPrint({
    t()
  })
  
  
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
