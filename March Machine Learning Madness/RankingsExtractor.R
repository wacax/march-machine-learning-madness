RankingsExtractor <- function(teams1, teams2, seasonVector, date = NULL){
  
  #load dataFrames
  tourneyResults <- read.csv(paste0(dataDirectory, 'tourney_results.csv'), header = TRUE, stringsAsFactors = FALSE)
  sagarinRanking <- read.csv(paste0(dataDirectory, 'sagp_weekly_ratings.csv'), header = TRUE, stringsAsFactors = FALSE)
  core33 <- read.csv(paste0(dataDirectory, 'ordinal_ranks_core_33.csv'), header = TRUE, stringsAsFactors = FALSE)
  nonCore <- read.csv(paste0(dataDirectory, 'ordinal_ranks_non_core.csv'), header = TRUE, stringsAsFactors = FALSE)
  seasons <- read.csv(paste0(dataDirectory, 'seasons.csv'), header = TRUE, stringsAsFactors = FALSE)
  
  if(date == NULL){
    date <- rep(min(tourneyResults$daynum), length(seasonVector))
  }
  
  anonFun <- function(teamInt, seasonInt, dateInt){
    #sagarin
    teamAndSeasonIdx <- sagarinRanking$season == seasonInt & sagarinRanking$team == teamInt
    ordinalRank <- sagarinRanking$orank[teamAndSeasonIdx]
    sagarinRating <- sagarinRanking$rating[teamAndSeasonIdx]
    rankIdx <- which.min(abs(sagarinRanking$rating_day_num[teamAndSeasonIdx] - dateInt))
    sagarinRankings <- cbind(ordinalRank[rankIdx], sagarinRating[rankIdx])
    
    #core33
    teamAndSeasonIdx <- core33$season == seasonInt & core33$team == teamInt
    ordinalRank <- core33[teamAndSeasonIdx, ]
    rankIdxs <- abs(core33$rating_day_num[teamAndSeasonIdx] - dateInt) %in% min(abs(core33$rating_day_num[teamAndSeasonIdx] - dateInt)) 
    ordinalRank <- ordinalRank[rankIdxs, ]
    namesOR <- ordinalRank$sys_name
    ordinalRank <- ordinalRank$orank
    names(ordinalRank) <- namesOR
    
    return(sagarinRanking)
  }
  
  
  
  
  
  
  dayZeroSeasons <- sapply(seasons$dayzero, anonFun <- function(string){
    return(as.Date(string, format = '%m/%d/%Y'))
  })
  
  dummy <- as.factor(c(as.character(seasonVector),  c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R", "S")))
  seasonVector <- as.numeric(dummy[1: length(seasonVector)])
  
  if (date == NULL){
    newDate <- sapply(seasonVector, anonFun <- function(seasonInt){
      return(dayZeroSeasons[seasonInt] + min(tourneyResults$daynum))
    })
  }else{    
    anonFun <- function(seasonInt, DateGame, dayZeroSeasons){
      return(dayZeroSeasons[seasonInt] + DateGame)
    }    
    newDate <- rep(0, length(date))
    for(i in 1:length(date)){
      newDate[i] <- anonFun(seasonVector[i], date[i], dayZeroSeasons)
    }    
  }
  
  anonFun <- function(seasonInt, DateGame, dayZeroSeasons){
    return(dayZeroSeasons[seasonInt] + DateGame)
  }
  dummy <- as.factor(c(as.character(sagarinRanking$season),  c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R", "S")))
  sagarinSeason <- as.numeric(dummy[1: length(sagarinRanking$season)])  
  SagarinDate <- rep(0, length(sagarinSeason))
  for(i in 1:length(sagarinSeason)){
    SagarinDate[i] <- anonFun(sagarinSeason[i], sagarinRanking$rating_day_num[i], dayZeroSeasons)    
  }
  
  
  
  
}