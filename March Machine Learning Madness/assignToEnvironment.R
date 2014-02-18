assignToEnvironment <- function(keys, envMatrix, environmentsName){
    
    for (i in 1:length(keys)){
      
      if (ncol(envMatrix) == 1){
        assign(keys[i], as.vector(envMatrix)[i], envir = environmentsName)
        
      }else{
      
      assign(keys[i], envMatrix[i, ], envir = environmentsName)
      }
      
    }
}
