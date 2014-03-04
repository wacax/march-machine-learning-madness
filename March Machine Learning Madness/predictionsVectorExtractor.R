predictionsVectorExtractor <- function(string, seasonResults, tourneyResults, tourneySeeds){
  
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
  
  
}