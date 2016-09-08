shinyServer(function(input, output) {

gameA <- function(p){
  WinLose <- sample(c(1,-1), size = 1, prob=c(p,(1-p)))
}


gameB <- function(CurrentCapital, p1, p2){
  WinLose <- 0
  if ((CurrentCapital %% 3) == 0){
    WinLose <- sample(c(1,-1), size = 1, prob= c(p1, (1-p1))) }else{
      WinLose <- sample(c(1,-1), size = 1, prob=c(p2, (1-p2)))}
  
  return(WinLose)
}


gameC <- function(LocalCopyCapital, p, p1, p2, gamma){
  switcher <- sample(c("A","B"), 1, prob = c(gamma, (1-gamma)))
  
  if(switcher == "A"){
    WinLose <- gameA(p)
  } else {
    WinLose <- gameB(LocalCopyCapital, p1, p2)
  }
  return(WinLose)
}


#output$SimGameA <- renderPlot({
output$SimGame <- renderPlot({
  par(mfrow = c(2,2))

MyCurrentCapitalA <- matrix(0, input$SimLength,input$replicates)

for (j in 1:input$replicates){
MyCurrentCapitalA[1,j] <- input$MyInitialCapital
for (i in 2:input$SimLength){
  if(MyCurrentCapitalA[i-1,j] > 0){
   WinLose <- gameA(input$p)
  MyCurrentCapitalA[i,j] <- MyCurrentCapitalA[i-1,j]+WinLose
  }else{MyCurrentCapitalA[i,j]<- 0}
}
}

matplot(MyCurrentCapitalA, type = "l", xlab = "Iteration", ylab = "Capital", main = "Game A")
#})


#output$SimGameB <- renderPlot({

MyCurrentCapitalB <- matrix(0, input$SimLength,input$replicates)

for (j in 1:input$replicates){
  MyCurrentCapitalB[1,j] <- input$MyInitialCapital
  for (i in 2:input$SimLength){
    if(MyCurrentCapitalB[i-1,j] > 0){
      WinLose <- gameB(MyCurrentCapitalB[i-1,j], input$p1, input$p2)
      MyCurrentCapitalB[i,j] <- MyCurrentCapitalB[i-1,j]+WinLose
    }else{MyCurrentCapitalB[i,j]<- 0}
  }
}

matplot(MyCurrentCapitalB, type = "l", xlab = "Iteration", ylab = "Capital", main = "Game B")
#})

#output$SimGameC <- renderPlot({
MyCurrentCapitalC <- matrix(0, input$SimLength,input$replicates)

for (j in 1:input$replicates){
  MyCurrentCapitalC[1,j] <- input$MyInitialCapital
  for (i in 2:input$SimLength){
    if(MyCurrentCapitalC[i-1,j] > 0){
      WinLose <- gameC(MyCurrentCapitalC[i-1,j], input$p, input$p1, input$p2, input$gamma)
      MyCurrentCapitalC[i,j] <- MyCurrentCapitalC[i-1,j]+WinLose
    }else{MyCurrentCapitalC[i,j]<- 0}
  }
}

matplot(MyCurrentCapitalC, type = "l", xlab = "Iteration", ylab = "Capital", main = "Game C")
})


})
