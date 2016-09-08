# ROC curve
rf_pred <- predict(modelRF2, newdata = testset, type = "prob")
nb_pred <- predict(modelNB2, newdata = testset, type = "prob")
lr_pred <- predict(modelLR2, newdata = testset, type = "prob")


predRF <- prediction(rf_pred[,2], testset$outcome)
perfRF <- performance(predRF, "tpr", "fpr")
plot(perfRF, main = "ROC curves for Randomforest,NaiveBayes,LogisticRegression")

# Generate an ROC curve for the NewBayes method
predNB <- prediction(nb_pred[,2], testset$outcome)
perf_nb <- performance(predNB, "tpr", "fpr")
plot(perf_nb, add = TRUE, col = "blue")

# Generate an ROC curve for the NewBayes method
predLR <- prediction(lr_pred[,2], testset$outcome)
perf_lr <- performance(predLR, "tpr", "fpr")
plot(perf_lr, add = TRUE, col = "red")

# Gnerate ROC curve for ICHScore

ichscore_pred = prediction(my.df$rev.ich, my.df$Outcome)
perf_ich <- performance(ichscore_pred, "tpr", "fpr")
plot(perf_ich, add = TRUE, col = "green")


# Add legends to plot
legend("right", legend = c("randomForest", "
                           nb", "lr","ICHscore"), bty = "n", cex = 1, lty = 1,
       col = c("black", "blue", "red","green"))
detach("package:caret", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
devtools::install_github("hadley/ggplot2")
devtools::install_github("sachsmc/plotROC")
detach("package:scales", unload=TRUE)

library(ggplot2)
library(plotROC)
test = data.frame(lr_pred[,2],rf_pred[,2],nb_pred[,2],testset$outcome)
longtest <- melt_roc(test, "outcome", c("lr", "rf","nb"))

modelNB =  train(as.factor(Outcome) ~ Age + Sex + MAP +   
                   RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                 data = trainset, # Use the trainSet dataframe as the training data
                 method = "nb",# Use the "random forest" algorithm
                 metric = "ROC",
                 trControl = fitControl
)

modelRF =  train(as.factor(Outcome) ~ Age + Sex + MAP +   
                   RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                 data = trainset, # Use the trainSet dataframe as the training data
                 method = "rf",# Use the "random forest" algorithm
                 metric = "ROC",
                 trControl = fitControl
)

modelLR <- train(as.factor(Outcome) ~ Age + Sex + MAP +   
                   RBS + GCS + tentorium + VE + Volume , # Survived is a function of the variables we decided to include
                 data = trainset, # Use the trainSet dataframe as the training data
                 method = "glm",# Use the "random forest" algorithm
                 family = "binomial",
                 metric = "ROC",
                 trControl = fitControl
)