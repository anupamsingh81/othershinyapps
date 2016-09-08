getwd()

library(devtools)
devtools::install_github("RevolutionAnalytics/AzureML")

library(AzureML)
ws <- workspace()
ws

# silly vignette (https://htmlpreview.github.io/?https://github.com/RevolutionAnalytics/AzureML/blob/master/vignettes/getting_started.html)

add <- function(x, y) {
  x + y
}

ws <- workspace()
api <- publishWebService(
  ws,
  fun = add, 
  name = "AzureML-vignette-silly",
  inputSchema = list(
    x = "numeric", 
    y = "numeric"
  ), 
  outputSchema = list(
    ans = "numeric"
  )
)

class(api)
names(api)
(helpPageUrl <- api$HelpLocation)

# Updating web service
api <- updateWebService(
  ws,
  fun = function(x, y) x - y,
  inputSchema = list(
    x = "numeric",
    y = "numeric"
  ),
  outputSchema = list(
    ans = "numeric"
  ),
  serviceId = api$WebServiceId   # <<-- Required to update!
)

# Discover services
(webservices <- services(ws, name = "AzureML-vignette-silly"))



# consume service
df <- data.frame(
  x = 1:5,
  y = 6:10
)
s <- services(ws, name = "AzureML-vignette-silly")
s <- tail(s, 1) # use the last published function, in case of duplicate function names
ep <- endpoints(ws, s)
consume(ep, df)

# Delete Service
deleteWebService(ws, name = "AzureML-vignette-silly")

#Machine ex.1
library(AzureML)
library(MASS)
library(gbm)

#Dataset Boston
str(Boston)
ws <- workspace()
test <- Boston[1:5, 1:13]

# Train Model
set.seed(123)
gbm1 <- gbm(medv ~ .,
            distribution = "gaussian",
            n.trees = 5000,
            interaction.depth = 8,
            n.minobsinnode = 1,
            shrinkage = 0.01,
            cv.folds = 5,
            data = Boston)
#new
best.iter <- gbm.perf(gbm1, method="cv", plot=FALSE)

# prediction function
mypredict <- function(newdata)
{
  require(gbm)
  predict(gbm1, newdata, best.iter)
}

# Example use of the prediction function, local check
print(mypredict(test))

# consume service
# Publish the service
ep <- publishWebService(ws = ws, fun = mypredict, name = "AzureML-vignette-gbm",
                        inputSchema = test)

# Consume test data, comparing with result above
print(consume(ep, test))

#Machine exp. 2
library(e1071)
data(iris)
str(iris)
testi <- iris[48:54, 1:4]

fit <- naiveBayes(Species~., data=iris) 
predict <- predict(fit, c(5.1,3.5,1.4,0.2))
predict[4]

predict(fit,testi)

# Define predictive function
predictSpecies <- function(newdata){
  require(e1071)
  predict <- predict(fit, newdata)
  }

# Locally check results of predictive function
print(predictSpecies(testi))

library(AzureML)
ws = workspace()
# consume service
# Publish the service
Jp <- publishWebService(ws = ws, fun = predictSpecies, name = "IrisPrediction",
                        inputSchema = testi)


# Consume test data, comparing with result above
print(consume(Jp, testi))
Jp

# ApiLocation = ApiPostURL
#ApiLocation
# https://ussouthcentral.services.azureml.net/workspaces/e11b178d93464e55b80e24aac765a027/services/267f332e64e9406e82cb2a71d5d5cf45/execute?api-version=2.0&details=true
# Deleter format =swagger
# Api key =Primary key
# l4VI9Q3lIDHU0Cf+pTRVcw42VRKL5nLxJDksVxOmAwFbxhRw6SmCct8zaQ9Zpl4DLGlgmrF46wye2XoOuwVMcw==

# deploying start

Next, we need to get the return values of the API. These include the API URL, API Key and Help Page URL.

(webservices <- services(ws, name = "IrisPrediction"))
names(Ip
