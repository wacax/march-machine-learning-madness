predictionsVectorExtractor <- function(string){
  
  arf <- strsplit(string, '_')  
  season <- sapply(arf, anonFun <- function(anonList){anonList[1]
  })
  home <- sapply(arf, anonFun <- function(anonList){anonList[2]
  })
  visitor <- sapply(arf, anonFun <- function(anonList){anonList[3]
  })
}