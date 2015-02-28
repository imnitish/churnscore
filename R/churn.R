churn <- function(input){
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
  to.drop <- c('State','Area.Code','Phone','Churn.')
  predictors <- newdata[,!(names(newdata) %in% to.drop)]
  yes.no.cols <- c("Int.l.Plan","VMail.Plan")
  predictors[yes.no.cols] <- predictors[yes.no.cols] == "yes"
  to.transform <- c("Account.Length", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge",    
                    "Eve.Mins","Eve.Calls", "Eve.Charge", "Night.Mins", "Night.Calls", "Night.Charge", "Intl.Mins",     
                    "Intl.Calls", "Intl.Charge", "CustServ.Calls")
  transform.cols <- predictors[,(names(predictors) %in% to.transform)]
  trans <- preProcess(transform.cols, method = c("BoxCox", "center", "scale"))
  predictors[to.transform] <- predict(trans, transform.cols)
  
  
  #safety_model is included with the package
  
  svmPredprob <- predict(svmFit, predictors, type = "prob")
  newdata$Churn <- svmPredprob$True
  return(newdata)
}