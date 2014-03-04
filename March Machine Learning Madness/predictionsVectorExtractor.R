predictionsVectorExtractor <- function(string, seasonResults, tourneyResults, tourneySeeds, historic, SportsReferenceData){
  
  arf <- strsplit(string, '_')  
  seasonVector <- sapply(arf, anonFun <- function(anonList){anonList[1]
  })
  home <- sapply(arf, anonFun <- function(anonList){anonList[2]
  })
  visitor <- sapply(arf, anonFun <- function(anonList){anonList[3]
  })
  
  homeSeeds <- rep(0, length(home))
  visitorSeeds <- rep(0, length(home))
  
    for(i in 1:length(home)){
    homeSeeds[i] <- tourneySeeds$seed[tourneySeeds$team == home[i] & tourneySeeds$season == seasonVector[i]]    
  }
  
  for(i in 1:length(visitor)){
    visitorSeeds[i] <- tourneySeeds$seed[tourneySeeds$team == visitor[i] & tourneySeeds$season == seasonVector[i]]    
  }
  
  homeSeeds <- as.numeric(gsub('[A-Za-z]', '', homeSeeds))
  visitorSeeds <- as.numeric(gsub('[A-Za-z]', '', visitorSeeds))
  
  
  #historic data of teams (Test)
  anonFun <- function(keyWin, keyLose, environment, vectorZeros, SportsReferenceData){
    if(sum(SportsReferenceData$id == keyWin) != 0 & sum(SportsReferenceData$id == keyLose) != 0){
      vectorWin <- get(as.character(keyWin), envir = environment)
      vectorLose <- get(as.character(keyLose), envir = environment)
      historicVector <- as.numeric(cbind(vectorWin, vectorLose))
    }else{
      historicVector <- c(vectorZeros, vectorZeros)
    }
  }
  
  vectorZeros <- rep(0, 15)
  testHistoric <- matrix(rep(0, length(string) * 30), nrow=length(string), ncol=30) 
  for(i in 1:length(string)){
    testHistoric[i, ] <- anonFun(home[i], visitor[i], historic.env, vectorZeros, SportsReferenceData)
  } 
  
}