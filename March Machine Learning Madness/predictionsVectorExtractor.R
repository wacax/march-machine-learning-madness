predictionsVectorExtractor <- function(string, seasonResults, tourneyResults, tourneySeeds, historicEnv, SportsReferenceData){
  
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
  
  #Matrix binding
  season <- seasonVector
  winSeeds <- homeSeeds
  loseSeeds <- visitorSeeds
  testResults <- as.data.frame(cbind(season, winSeeds, loseSeeds))
  #colnames <- names(SportsReferenceData); colnames <- colnames[c(-1, -2, -3)]
  tourney <- rep(1, nrow(testResults))
  testResults <- cbind(testResults, tourney, testHistoric)
  colnames <- c("tourney", "From","To","Yrs","G","W","L","W.L.","SRS","SOS","AP","CREG","CTRN","NCAA","FF","NC","From2","To2","Yrs2","G2","W2","L2","W.L.2","SRS2","SOS2","AP2","CREG2","CTRN2","NCAA2","FF2","NC2")
  colnames <- c(names(testResults[1:3]), colnames)
  names(testResults) <- colnames
  
  dummy <- as.factor(c(as.character(testResults$season),  c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R")))
  testResults['season'] <- dummy[1: length(string)]
  
  return(testResults)
}