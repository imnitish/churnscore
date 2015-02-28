## http://blog.yhathq.com/posts/predicting-customer-churn-with-sklearn.html
## But with R and caret package

library(caret)
dataframe <- read.csv("/Users/juaneshutte/Tech/Python/yhat-examples/churn/data/rchurn.csv")
classes <- factor(dataframe$Churn.)
to.drop <- c('State','Area.Code','Phone','Churn.')
predictors <- dataframe[,!(names(dataframe) %in% to.drop)]
yes.no.cols <- c("Int.l.Plan","VMail.Plan")
predictors[yes.no.cols] <- predictors[yes.no.cols] == "yes"

# Apply the transformations:
to.transform <- c("Account.Length", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge",    
                   "Eve.Mins","Eve.Calls", "Eve.Charge", "Night.Mins", "Night.Calls", "Night.Charge", "Intl.Mins",     
                  "Intl.Calls", "Intl.Charge", "CustServ.Calls")
transform.cols <- predictors[,(names(predictors) %in% to.transform)]
trans <- preProcess(transform.cols, method = c("BoxCox", "center", "scale"))
predictors[to.transform] <- predict(trans, transform.cols)

# Create stratified random splits of the data
# Set the random number seed so we can reproduce the results
set.seed(199)
trainingRows <- createDataPartition(classes, p = .70, list= FALSE)

# Subset the data into objects for training using integer sub-setting.
trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]

testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]

set.seed(1056)
svmFit <- train(trainClasses ~ .,
                  data = trainPredictors,
                  method = "svmRadial",
                  tuneLength = 10,
                  trControl = trainControl(method = "repeatedcv",
                  repeats = 5, classProbs = TRUE))
svmFit
plot(svmFit, scales = list(x = list(log = 2)))

svmPred <- predict(svmFit, testPredictors)
confusionMatrix(svmPred, testClasses)
svmPredprob <- predict(svmFit, testPredictors, type = "prob")
head(svmPredprob)

library(pROC)
svmFitROC <- roc(predictor = svmPredprob$True,
               response = testClasses,
               levels = rev(levels(testClasses)))

#Store the model as a data object
save(svmFit, file="data/svmFit.rda")

#test scoring with CSV file input
library(churnscore)
setwd("/Users/juaneshutte/Tech/Python/yhat-examples/churn/")
churn(input = "test_churn_ocpu_raw_one.csv")



###Scoring using 1 data point
###Further testing with different Preprocess method (model.matrix) to create dummy variables
###Modeling and Testing
library(caret)
dataframe <- read.csv("/Users/juaneshutte/Tech/Python/yhat-examples/churn/data/rchurn.csv")
Churn. <- factor(dataframe$Churn.)
to.drop <- c('State','Area.Code','Phone')
predictors <- dataframe[,!(names(dataframe) %in% to.drop)]

set.seed(199)
trainingRows <- createDataPartition(Churn., p = 0.7)[[1]]
training <- predictors[trainingRows, ]
testing <- predictors[-trainingRows, ]

#Create dummy variables
trainingInd <- data.frame(model.matrix(Churn. ~ .,
                                         data = training))[,-1]
testingInd <- data.frame(model.matrix(Churn. ~ .,
                                           data = testing))[,-1]
## Add the outcome back into the data set
trainingInd$Churn. <- training$Churn.
testingInd$Churn. <- testing$Churn.

set.seed(1056)
svmFitone <- train(trainingInd$Churn. ~ .,
                data = trainingInd,
                method = "svmRadial",
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5, classProbs = TRUE))

predictedProbs <- predict(svmFitone, trainingInd,
                          type = "prob")
svmFitone
svmPredprob <- predict(svmFitone, testingInd, type = "prob")
head(svmPredprob)


#Store the model as a data object
setwd("/Users/juaneshutte/Tech/Git/churnscore")
save(svmFitone, file="data/svmFitone.rda")


###Production
library(caret)
setwd("/Users/juaneshutte/Tech/Python/yhat-examples/churn/")
test <- read.csv("/Users/juaneshutte/Tech/Python/yhat-examples/churn/test_churn_ocpu_raw_one.csv",sep=",",header=T)
to.drop <- c('State','Area.Code','Phone')
df <- test[,!(names(test) %in% to.drop)]
library(plyr)
df <- rbind(df,
data.frame(Account.Length=50, Int.l.Plan="yes", VMail.Plan="no", VMail.Message=15, Day.Mins=10, Day.Calls=5, Day.Charge=20,    
           Eve.Mins=4, Eve.Calls=20, Eve.Charge=30, Night.Mins=60, Night.Calls=2, Night.Charge=300, Intl.Mins=15,     
           Intl.Calls=1, Intl.Charge=10, CustServ.Calls=0),
data.frame(Account.Length=200, Int.l.Plan="no", VMail.Plan="yes", VMail.Message=25, Day.Mins=30, Day.Calls=3, Day.Charge=60,    
           Eve.Mins=1, Eve.Calls=5, Eve.Charge=15, Night.Mins=80, Night.Calls=5, Night.Charge=60, Intl.Mins=5,     
           Intl.Calls=3, Intl.Charge=30, CustServ.Calls=1))
df['Churn.'] <- as.factor('False')
predictors1 <- names(df)[names(df) != "Churn."]
dfInd <- data.frame(model.matrix(Churn. ~ .,
                                       data = df))[,-1]
dfInd$Churn. <- df$Churn.
svmPreddf <- predict(svmFitone, newdata=dfInd, type = "prob")
svmPreddf
setwd("/Users/juaneshutte/Tech/Python/yhat-examples/churn/")
churnone(input = "test_churn_ocpu_raw_one.csv")


