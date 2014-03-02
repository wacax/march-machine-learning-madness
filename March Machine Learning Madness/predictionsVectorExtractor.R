predictionsVectorExtractor <- function(string, seasonResults, tourneyResults, tourneySeeds){
  
  arf <- strsplit(string, '_')  
  season <- sapply(arf, anonFun <- function(anonList){anonList[1]
  })
  home <- sapply(arf, anonFun <- function(anonList){anonList[2]
  })
  visitor <- sapply(arf, anonFun <- function(anonList){anonList[3]
  })
  
  homeSeed <- sapply(home, anonFun <- function(homeItem, season){
    tourneySeeds$seed[tourneySeeds$team == homeItem & tourneySeeds$season == season]    
  })
  visitorSeed <- sapply(visitor, anonFun <- function(awayItem, season){
    tourneySeeds$seed[tourneySeeds$team == awayItem & tourneySeeds$season == season]    
  })
}