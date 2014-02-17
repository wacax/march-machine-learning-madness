assignToEnvironment <- function(keys, envMatrix, environmentsName){
  for (i in 1: length(keys)){
    assign(keys[i], envMatrix[i, ], envir = environmentsName)
    }
}
