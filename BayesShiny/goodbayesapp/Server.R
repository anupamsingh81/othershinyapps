
library("BEST")
library(BayesFactor)
library(shiny)

shinyServer(function(input, output) {

  Data <- reactive({
#  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)



  })




output$contents <- renderTable({ 
  my.df <- Data()
  if (is.null(my.df)){return(NULL)} 
  my.df})



output$plot <- renderPlot({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  z= c(input$r,input$s)
  a= c(input$t)
  d = c(input$u)
  e = c(input$v)
  BESTout<-BESTmcmc(my.df[,1],my.df[,2],numSavedSteps = 20000)
  plot(BESTout,which = "mean" ,credMass = e, ROPE =z, compVal = d) })


output$plot2 <- renderPlot({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  z= c(input$r,input$s)
  a= c(input$t)
  d = c(input$u)
  e = c(input$v)
  BESTout<-BESTmcmc(my.df[,1],my.df[,2],numSavedSteps = 20000)
  plot(BESTout,which = "eff" ,credMass = e, ROPE =z, compVal = 0.3) })

bayes <- reactive({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  z= c(input$r,input$s)
  a= c(input$t)
  d = c(input$u)
  e = c(input$v)
  BESTout<-BESTmcmc(my.df[,1],my.df[,2],numSavedSteps = 20000)
  summary(BESTout,credMass= e, ROPEm= z, compValm =d ,compValeff=1,
          ROPEeff=c(-0.2,0.2))  
})


bayessummary <- reactive({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  BESTout<-BESTmcmc(my.df[,1],my.df[,2],numSavedSteps = 20000)
  summary(BESTout) 
})
t <- reactive({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  k = my.df[,1]
  l =  my.df[,2]
  score <- c(k, l)
  group <- factor(c(rep("Data 1", length(k)), rep("Data 2", length(l))))
  
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

output$bayessummary.out <- renderPrint({
  bayessummary()
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

