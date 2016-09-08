library("BayesianFirstAid")

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
  fit <- bayes.t.test(my.df[,1]~my.df[,2], )
  plot(fit)})
output$summary <- renderPrint({
  summary <- summary(fit)
  if (is.null(summary)){return(invisible())}
  ttestoutlog <- reactive({
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    if (min(my.df[,1]) <= 0 ){return(NULL)}
    t.test(log(my.df[,1])~my.df[,2], var.equal = TRUE)})
  



output$logplot <- renderPlot({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
    if (min(my.df[,1]) <= 0 ){return(NULL)}
  boxplot(log(my.df[,1])~my.df[,2], xlab = names(my.df)[1], ylab = names(my.df)[1], 
          main = paste("Study on ", names(my.df)[2]), col = "brown")})


ttestout <- reactive({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  t.test(my.df[,1]~my.df[,2], var.equal = TRUE)
})


ttestoutlog <- reactive({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
    if (min(my.df[,1]) <= 0 ){return(NULL)}
  t.test(log(my.df[,1])~my.df[,2], var.equal = TRUE)
})


output$tvalue <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(invisible())}
  vals$statistic
})


output$tvaluelog <- renderPrint({
  vals <- ttestoutlog()
  if (is.null(vals)){return(invisible())}
  vals$statistic
})


output$samplemeans1 <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(invisible())}
  vals$estimate[1]
  
})

output$samplemeans1log <- renderPrint({
  vals <- ttestoutlog()
  if (is.null(vals)){return(invisible())}
  vals$estimate[1]
  
})



output$samplemeans2 <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(invisible())}
  vals$estimate[2]
  
})

output$samplemeans2log <- renderPrint({
  vals <- ttestoutlog()
  if (is.null(vals)){return(invisible())}
  vals$estimate[2]
  
})



output$pvalue <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(invisible())}
  vals$p.value
  
})


output$pvaluelog <- renderPrint({
  vals <- ttestoutlog()
  if (is.null(vals)){return(invisible())}
  vals$p.value
  
})



output$fivenum <- renderTable({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  
  quantiles <- tapply(my.df[,1], my.df[,2], quantile, c(0.25, 0.5, 0.975))
  mins <- tapply(my.df[,1], my.df[,2], min)
  maxs <- tapply(my.df[,1], my.df[,2], max)
  fivenum <- data.frame(min = c(mins[1],mins[2] ), lower=c(unlist(quantiles[1])[1],unlist(quantiles[2])[1] ), 
                        median=c(unlist(quantiles[1])[2],unlist(quantiles[2])[2] ), upper=c(unlist(quantiles[1])[3],unlist(quantiles[2])[3] ),
                        max = c(maxs[1],maxs[2] ))
  return(fivenum)
})



output$fivenumlog <- renderTable({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
    if (min(my.df[,1]) <= 0 ){return(NULL)}
  quantiles <- tapply(log(my.df[,1]), my.df[,2], quantile, c(0.25, 0.5, 0.975))
  mins <- tapply(log(my.df[,1]), my.df[,2], min)
                 maxs <- tapply(log(my.df[,1]), my.df[,2], max)
                 fivenum <- data.frame(min = c(mins[1],mins[2] ), lower=c(unlist(quantiles[1])[1],unlist(quantiles[2])[1] ), 
                                       median=c(unlist(quantiles[1])[2],unlist(quantiles[2])[2] ), upper=c(unlist(quantiles[1])[3],unlist(quantiles[2])[3] ),
                                       max = c(maxs[1],maxs[2] ))
                 return(fivenum)
})





output$parametric <- renderTable({
  my.df <- Data()
  if (is.null(my.df)){return(NULL)}
  means <- tapply(my.df[,1], my.df[,2], mean)
  sds <- tapply(my.df[,1], my.df[,2], sd)
  ses<- tapply(my.df[,1], my.df[,2], function(x) sd(x)/sqrt(length(x)))
  parametric <- data.frame(mean = c(means[1],means[2] ), sd=c(sds[1], sds[2]), se=c(ses[1], ses[2]))
  return(parametric)
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

