customLogLoss <- function(actual, predicted){

  logPred <- log(predicted)
  logPred[logPred == -Inf] <- -20
  
  return(-(sum(actual * logPred + (1 - actual) * log(1 - predicted)) / length(actual)))
  

}