testFeaturesExtractor <- function(string){
  
  MasterData <- read.csv(paste0(dataDirectory, 'MasterData.csv'), header = TRUE, stringsAsFactors = FALSE)
    
  indexesVector <- sapply(MasterData, anonFun <- function(inputString){
    which(MasterData$Matchup == inputString)
  })
  
  MasterData <- MasterData[indexesVector, ]
  return(MasterData)  
}