library(caret)

library(ggplot2)

library(ROCR)








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
  
  
  ROC <- reactive({
    
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    
    Age<- my.df$Age
    
    MAP<- my.df$MAP
    
    RBS<- my.df$RBS
    
    GCS<- my.df$GCS
    
    
    my.df$GCSscore2 =  ifelse(my.df$GCS<=4,2,ifelse(my.df$GCS<=12,1,ifelse(my.df$GCS<=15,0)))
    
    my.df$Age2 = ifelse(my.df$Age<80,0,1)
    
    
    
    ## Calculation of Revised ICH score
    
    
    
    my.df$rev.ich = my.df$VE + my.df$Volume + my.df$tentorium + my.df$Age2+ my.df$GCSscore2
    
    
    
    # Plotting ROC curve and optimal cut off
    
    
    
    library(ROCR)
    
    opt.cut = function(perf, pred){
      
      cut.ind = mapply(FUN=function(x, y, p){
        
        d = (x - 0)^2 + (y-1)^2
        
        ind = which(d == min(d))
        
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
          
          cutoff = p[[ind]])
        
      }, perf@x.values, perf@y.values, pred@cutoffs)
      
    }
    
  
    
    
    # creating prediction and performance objects
    
    
    
    pred = prediction(my.df$rev.ich, my.df$Outcome)
    
    perf <- performance(pred, "tpr", "fpr")
    
    
    # Optimal cut off design
    
    x = (opt.cut(perf,pred ))
    
  
  })
  
  output$roc.out <- renderPrint({ 
    
    ROC() 
    
  })
  
    
  output$plot <- renderPlot({
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    
    
    # Handling Variables
    
    Age<- my.df$Age
    
    MAP<- my.df$MAP
    
    RBS<- my.df$RBS
    
    GCS<- my.df$GCS
    
    Outcome<- my.df$Outcome
    Outcome= as.factor(Outcome)
    
    Volume<- my.df$Volume
    Volume= as.factor(Volume)
    
    VE<- my.df$VE
    VE = as.factor(VE)
    
    tentorium<- my.df$tentorium
    tentorium= as.factor(tentorium)
    
    
    
    Sex<- my.df$Sex
    Sex = as.factor(Sex)
    
    Midlineshift<- my.df$Midlineshift
    Midlineshift = as.factor(Midlineshift)
    
    
    
    
    
    # creating a data frame of variables
    
    
    
    df = data.frame(Age,Sex,MAP,RBS,GCS,Volume,VE,Outcome,tentorium,Midlineshift)
    
    
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      
      library(grid)
      
      
      
      # Make a list from the ... arguments and plotlist
      
      plots <- c(list(...), plotlist)
      
      
      
      numPlots = length(plots)
      
      
      
      # If layout is NULL, then use 'cols' to determine layout
      
      if (is.null(layout)) {
        
        # Make the panel
        
        # ncol: Number of columns of plots
        
        # nrow: Number of rows needed, calculated from # of cols
        
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         
                         ncol = cols, nrow = ceiling(numPlots/cols))
        
      }
      
      
      
      if (numPlots==1) {
        
        print(plots[[1]])
        
        
        
      } else {
        
        # Set up the page
        
        grid.newpage()
        
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        
        
        # Make each plot, in the correct location
        
        for (i in 1:numPlots) {
          
          # Get the i,j matrix positions of the regions that contain this subplot
          
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          
                                          layout.pos.col = matchidx$col))
          
        }
        
      }
      
    }
    
    
    
    
    
    
    
    Ap =ggplot(aes(x = Outcome , y = Age), data = df) + geom_boxplot (aes(fill= Outcome))
    
    Bp = ggplot(aes(x = Outcome , y = RBS), data = df) + geom_boxplot (aes(fill= Outcome))
    
    Cp = ggplot(aes(x = Outcome , y = GCS), data = df) + geom_boxplot (aes(fill= Outcome))
    
    Dp = ggplot(aes(x = Outcome , y = MAP), data = df) + geom_boxplot (aes(fill= Outcome))
    
    
    
    
    
    
    
    
    
    
    
    # Plotting categorical by categorical variable
    
    Ep = ggplot(df, aes(Outcome, ..count..)) + geom_bar(aes(fill = Volume), position = "dodge")
    
    Fp =ggplot(df, aes(Outcome, ..count..)) + geom_bar(aes(fill = VE), position = "dodge")
    
    Gp = ggplot(df, aes(Outcome, ..count..)) + geom_bar(aes(fill = tentorium), position = "dodge")
    
    Hp = ggplot(df, aes(Outcome, ..count..)) + geom_bar(aes(fill = Sex), position = "dodge")
    
    
    
    
    
    multiplot(Ap , Bp, Cp, Dp,Ep,Fp,Gp,Hp, cols=2) })
  
  
  k <- reactive({
    
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    
    # Handling Variables
    
    
    
    
    Age<- my.df$Age
    
    MAP<- my.df$MAP
    
    RBS<- my.df$RBS
    
    GCS<- my.df$GCS
    
    Outcome<- my.df$Outcome
    Outcome= as.factor(Outcome)
    
    Volume<- my.df$Volume
    Volume= as.factor(Volume)
    
    VE<- my.df$VE
    VE = as.factor(VE)
    
    tentorium<- my.df$tentorium
    tentorium= as.factor(tentorium)
    
    
    
    Sex<- my.df$Sex
    Sex = as.factor(Sex)
    
    Midlineshift<- my.df$Midlineshift
    Midlineshift = as.factor(Midlineshift)
    
    
    
    # creating a data frame of variables
    
    
    
    df = data.frame(Age,Sex,MAP,RBS,GCS,Volume,VE,Outcome,tentorium,Midlineshift)
    
    
    
    # Cross tabs
    
    
    
    # crosstabs for categorical variables
    
    pc = table(df[,c("Outcome", "Sex")])
    
    qc = table(df[,c("Outcome", "VE")])
    
    rc = table(df[,c("Outcome", "tentorium")])
    
    sc  =table(df[,c("Outcome", "Volume")])
    
    
    
    
    
    # t.test
    
    
    
    Age.t <- t.test(Age~ Outcome, var.equal=TRUE) 
    
    RBS.t <- t.test(RBS~ Outcome, var.equal= TRUE) 
    
    GCS.t <- t.test(GCS~ Outcome, var.equal=TRUE) 
    
    MAP.t <- t.test(MAP~ Outcome, var.equal=TRUE)
    
    
    
    return(list(pc,qc,rc,sc,Age.t,RBS.t,GCS.t,MAP.t)) 
    
  }) 
  
  
  
  output$k.out <- renderPrint({ 
    
    k() 
    
  })
  
  LR <- reactive({
    
    my.df <- Data()
    if (is.null(my.df)){return(NULL)}
    
    Age<- my.df$Age
    
    MAP<- my.df$MAP
    
    RBS<- my.df$RBS
    
    GCS<- my.df$GCS
    
    my.df$Outcome = ifelse(my.df$Outcome==1,"Dead","Alive")
    
    Outcome<- my.df$Outcome
    Outcome= as.factor(Outcome)
    
    Volume<- my.df$Volume
    Volume= as.factor(Volume)
    
    VE<- my.df$VE
    VE = as.factor(VE)
    
    tentorium<- my.df$tentorium
    tentorium= as.factor(tentorium)
    
    
    
    Sex<- my.df$Sex
    Sex = as.factor(Sex)
    
    Midlineshift<- my.df$Midlineshift
    Midlineshift = as.factor(Midlineshift)
    
    # creating a data frame of variables
    
    
    
    df = data.frame(Age,Sex,MAP,RBS,GCS,Volume,VE,Outcome,tentorium,Midlineshift)
    # Start Machine Learning
    
    
    
    # create trainset and test set
    
    
    
    trainindex <- createDataPartition(df$Outcome, p=0.75, list=FALSE) 
    
    trainset <- df[trainindex,] 
    
    testset <- df[-trainindex,]
    
    # training and boot sampling can be optimized like
    fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5,
                               ## Estimate class probabilities 
                               classProbs = TRUE,
                               ## Evaluate performance using 
                               ## the following function
                               summaryFunction = twoClassSummary)
    
    
    # ROCmetric
    
    modelRF <- train(as.factor(Outcome) ~ Age + Sex + MAP +   
                       RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                     data = trainset, # Use the trainSet dataframe as the training data
                     method = "rf",# Use the "random forest" algorithm
                     metric = "ROC",
                     trControl = fitControl)
    
    
    modelLR <- train(as.factor(Outcome) ~ Age + Sex + MAP +   
                       RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                     data = trainset, # Use the trainSet dataframe as the training data
                     method = "glm",# Use the "random forest" algorithm
                     family = "binomial",
                     metric = "ROC",
                     trControl = fitControl
    )
    
    
    
    modelNB <- train(as.factor(Outcome) ~ Age + Sex + MAP +   
                       RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                     data = trainset, # Use the trainSet dataframe as the training data
                     method = "nb",# Use the "random forest" algorithm
                     metric = "ROC",
                     trControl = fitControl
    )
    
    
    
    
    # comparative Results in ROC
    results <- resamples(list( LR = modelLR, RF = modelRF , NB = modelNB))
    e9 = summary(results)
    
    
    
    
    
    
    
    temps = data.frame(Age <- c(input$a),
                       
                       Sex <- as.factor(input$b),
                       
                       MAP <- c(input$c),
                       
                       RBS <- c(input$e),
                       
                       GCS <- c(input$d),
                       
                       
                       
                       tentorium <- as.factor(input$f),
                       
                       VE <- as.factor(input$h),
                       
                       Volume <- as.factor(input$g)
                       
    )
    
    i =  ifelse(input$d<=4,2,ifelse(input$d<=12,1,ifelse(input$d<=15,0)))
    
    j = ifelse(input$a<80,0,1)
    
    
    
    ## Calculation of Revised ICH score
    
    
    
    k =  input$g + input$h + input$f + i + j
    
    
    newich = ifelse(k<=3,"Alive","Dead")
    
    
    temps$LR <- predict(modelLR, newdata = temps)
    temps$RF<- predict(modelRF, newdata = temps)
    
    
    
    
    
    
    
    
    e1= print ( " Predictions made by Logistic  Regression machine Algorithm  " )
    
    e7= print ( " Predictions made by Random Forest machine Algorithm  " )
    
    e8 = print ( " ICH score based on input and Predictions made by it  " )
    
    p = list(modelLR,modelNB,modelRF,e9,e1,temps$LR,e7,temps$RF,e8,newich) 
    
    return(list(p))
    
  }) 
    
    
    
  output$lr.out <- renderPrint({ 
    
    LR() 
    
  })
  
  
  
  #
  
  
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




  