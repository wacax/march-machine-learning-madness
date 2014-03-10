RankingsExtractor <- function(teams1, teams2, seasonVector, dateVec = NULL){
  
  #load dataFrames
  tourneyResults <- read.csv(paste0(dataDirectory, 'tourney_results.csv'), header = TRUE, stringsAsFactors = FALSE)
  sagarinRanking <- read.csv(paste0(dataDirectory, 'sagp_weekly_ratings.csv'), header = TRUE, stringsAsFactors = FALSE)
  core33 <- read.csv(paste0(dataDirectory, 'ordinal_ranks_core_33.csv'), header = TRUE, stringsAsFactors = FALSE)
  nonCore <- read.csv(paste0(dataDirectory, 'ordinal_ranks_non_core.csv'), header = TRUE, stringsAsFactors = FALSE)
  seasons <- read.csv(paste0(dataDirectory, 'seasons.csv'), header = TRUE, stringsAsFactors = FALSE)
  
  if(length(dateVec) == 0){
    dateVec <- rep(min(tourneyResults$daynum), length(seasonVector))
  }
  
  #ranking extractor
  rankingExtract <- function(teamInt, seasonInt, dateInt){
    #sagarin
    teamAndSeasonIdx <- sagarinRanking$season == seasonInt & sagarinRanking$team == teamInt
    if (sum(teamAndSeasonIdx) == 0){
      sagarinRankings <- c(NA, NA)  
      names(sagarinRankings) <- c('ordinalRankSagarin', 'sagarinRating')
    }else{
      ordinalRankSagarin <- sagarinRanking$orank[teamAndSeasonIdx]
      sagarinRating <- sagarinRanking$rating[teamAndSeasonIdx]
      rankIdx <- which.min(abs(sagarinRanking$rating_day_num[teamAndSeasonIdx] - dateInt))
      sagarinRankings <- cbind(ordinalRankSagarin[rankIdx], sagarinRating[rankIdx])
      names(sagarinRankings) <- c('ordinalRankSagarin', 'sagarinRating')
    }
    
    #core33
    namesOR <- unique(core33$sys_name)
    lenOR <- length(unique(core33$sys_name))
    seasonIdx <- core33$season == seasonInt
    if (sum(seasonIdx) == 0){
      ordinalRank <- rep(NA, length(namesOR))
      names(ordinalRank) <- namesOR
    }else{
      coreCut <- core33[seasonIdx, ]
      teamIdx <- coreCut$team == teamInt
      ordinalRank <- coreCut[teamIdx, ]
      rankIdxs <- abs(ordinalRank$rating_day_num - dateInt) %in% min(abs(ordinalRank$rating_day_num - dateInt)) 
      ordinalRank <- ordinalRank[rankIdxs, ]
      namesOR <- ordinalRank$sys_name
      ordinalRank <- ordinalRank$orank
      if(length(ordinalRank) < lenOR){
        ordinalRank <- rep(NA, lenOR)    
      }else{
        names(ordinalRank) <- namesOR
      }      
    }
    
    return(c(sagarinRankings, ordinalRank))
  }
  
  coreRankings <- matrix(rep(0, length(seasonVector) * (length(unique(core33$sys_name)) + 2)), 
                         nrow = length(seasonVector), ncol = (length(unique(core33$sys_name)) + 2))
  for(i in 1:length(seasonVector)){
    coreRankings[i, ] <- rankingExtract(teams1[i], seasonVector[i], dateVec[i])    
  }
  
  names(coreRankings) <- unique(core33$sys_name)
  
  coreRankings2 <- matrix(rep(0, length(seasonVector) * (length(unique(core33$sys_name)) + 2)), 
                         nrow = length(seasonVector), ncol = (length(unique(core33$sys_name)) + 2))
  for(i in 1:length(seasonVector)){
    coreRankings2[i, ] <- rankingExtract(teams2[i], seasonVector[i], dateVec[i])    
  }
  
  names(coreRankings) <- c("AP2","DUN2","MOR2","POM2","RPI2","SAG2","SE2","USA2","WLK2","BOB2","RTH2","WOL2","COL2","DOL2","CNG2","DES2","DC2","WIL2","DOK2","PIG2","MB2","RTR2","CPR2","REW2","STH2","SPW2","PGH2","CPA2","RTB2","BPI2","NOL2","DCI","LMC2")
    
  return(as.data.frame(coreRankings))
}