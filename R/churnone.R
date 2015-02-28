churnone <- function(input){
  #Function score 1 data input, adds dummy data, scales and transforms and then predicts, returning row number 1
  #input can either be csv file or data    
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input, sep=",",header=T)
  } else {
    as.data.frame(input)
  }
  stopifnot("State" %in% names(newdata))
  stopifnot("Account.Length" %in% names(newdata))
  stopifnot("Area.Code" %in% names(newdata))
  stopifnot("Phone" %in% names(newdata))
  stopifnot("Int.l.Plan" %in% names(newdata))
  stopifnot("VMail.Plan" %in% names(newdata))
  stopifnot("VMail.Message" %in% names(newdata))
  stopifnot("Day.Mins" %in% names(newdata))
  stopifnot("Day.Calls" %in% names(newdata))
  stopifnot("Day.Charge" %in% names(newdata))
  stopifnot("Eve.Mins" %in% names(newdata))
  stopifnot("Eve.Calls" %in% names(newdata))
  stopifnot("Eve.Charge" %in% names(newdata))
  stopifnot("Night.Mins" %in% names(newdata))
  stopifnot("Night.Calls" %in% names(newdata))
  stopifnot("Night.Charge" %in% names(newdata))
  stopifnot("Intl.Mins" %in% names(newdata))
  stopifnot("Intl.Calls" %in% names(newdata))
  stopifnot("Intl.Charge" %in% names(newdata))
  stopifnot("CustServ.Calls" %in% names(newdata))
  
  
  require(caret, quietly=TRUE)
  to.drop <- c('State','Area.Code','Phone')
  df <- newdata[,!(names(newdata) %in% to.drop)]
  library(plyr)
  df <- rbind(df,
              data.frame(Account.Length=50, Int.l.Plan="yes", VMail.Plan="no", VMail.Message=15, Day.Mins=10, Day.Calls=5, Day.Charge=20,    
                         Eve.Mins=4, Eve.Calls=20, Eve.Charge=30, Night.Mins=60, Night.Calls=2, Night.Charge=300, Intl.Mins=15,     
                         Intl.Calls=1, Intl.Charge=10, CustServ.Calls=0),
              data.frame(Account.Length=200, Int.l.Plan="no", VMail.Plan="yes", VMail.Message=25, Day.Mins=30, Day.Calls=3, Day.Charge=60,    
                         Eve.Mins=1, Eve.Calls=5, Eve.Charge=15, Night.Mins=80, Night.Calls=5, Night.Charge=60, Intl.Mins=5,     
                         Intl.Calls=3, Intl.Charge=30, CustServ.Calls=1))
  
  df['Churn'] <- as.factor('False')
  predictors1 <- names(df)[names(df) != "Churn"]
  dfInd <- data.frame(model.matrix(Churn ~ .,
                                   data = df))
  dfInd$Churn <- df$Churn
  
  #predict with new data
  svmPreddf <- predict(svmFitone, newdata=dfInd, type = "prob")
  newdata['Churn'] <- head(svmPreddf$True,1)
  
  return(newdata)
}