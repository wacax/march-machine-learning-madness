RankingsExtractor <- function(teams1, teams2, seasonVector, dateVec = NULL){
  
  #load dataFrames
  tourneyResults <- read.csv(paste0(dataDirectory, 'tourney_results.csv'), header = TRUE, stringsAsFactors = FALSE)
  sagarinRanking <- read.csv(paste0(dataDirectory, 'sagp_weekly_ratings.csv'), header = TRUE, stringsAsFactors = FALSE)
  core33 <- read.csv(paste0(dataDirectory, 'ordinal_ranks_core_33.csv'), header = TRUE, stringsAsFactors = FALSE)
  nonCore <- read.csv(paste0(dataDirectory, 'ordinal_ranks_non_core.csv'), header = TRUE, stringsAsFactors = FALSE)
  seasons <- read.csv(paste0(dataDirectory, 'seasons.csv'), header = TRUE, stringsAsFactors = FALSE)
  tourneySeeds <- read.csv(paste0(dataDirectory, 'tourney_seeds.csv'), header = TRUE, stringsAsFactors = FALSE)
    
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
    originalNamesOR <- sort(unique(core33$sys_name))
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
        names(ordinalRank) <- namesOR
        ordinalRankNew <- rep(NA, lenOR)
        for(ii in 1:length(ordinalRank)){
          ordinalRankNew[originalNamesOR %in% names(ordinalRank[ii])] <- ordinalRank[ii]
        }
        ordinalRank <- ordinalRankNew
        names(ordinalRank) <- originalNamesOR
      }else{
        names(ordinalRank) <- namesOR
      }      
    }
    if(length(c(sagarinRankings, ordinalRank)) > lenOR + 2){
      return(rep(NA, lenOR + 2))
    }else{
      return(c(sagarinRankings, ordinalRank))
    }
  }
  
  coreRankings <- matrix(rep(0, length(seasonVector) * (length(unique(core33$sys_name)) + 2)), 
                         nrow = length(seasonVector), ncol = (length(unique(core33$sys_name)) + 2))
  for(i in 1:length(seasonVector)){
    coreRankings[i, ] <- rankingExtract(teams1[i], seasonVector[i], dateVec[i])    
  }
  coreRankings <- as.data.frame(coreRankings)
  names(coreRankings) <- c("sagarinRankings","ordinalRank", sort(unique(core33$sys_name)))
  
  coreRankings2 <- matrix(rep(0, length(seasonVector) * (length(unique(core33$sys_name)) + 2)), 
                         nrow = length(seasonVector), ncol = (length(unique(core33$sys_name)) + 2))
  for(i in 1:length(seasonVector)){
    coreRankings2[i, ] <- rankingExtract(teams2[i], seasonVector[i], dateVec[i])    
  }
  coreRankings2 <- as.data.frame(coreRankings2)
  names(coreRankings) <- c("sagarinRankings","ordinalRank","AP2","DUN2","MOR2","POM2","RPI2","SAG2","SE2","USA2","WLK2","BOB2","RTH2","WOL2","COL2","DOL2","CNG2","DES2","DC2","WIL2","DOK2","PIG2","MB2","RTR2","CPR2","REW2","STH2","SPW2","PGH2","CPA2","RTB2","BPI2","NOL2","DCI","LMC2")
  
  #non-Core  
  nonCorerankingExtract <- function(teamInt, seasonInt, dateInt){
    namesOR <- unique(nonCore$sys_name)
    originalNamesOR <- sort(unique(nonCore$sys_name))
    lenOR <- length(unique(nonCore$sys_name))
    seasonIdx <- nonCore$season == seasonInt
    
    if (sum(seasonIdx) == 0){
      ordinalRank <- rep(NA, length(namesOR))
      names(ordinalRank) <- namesOR
    }else{
      nonCoreCut <- nonCore[seasonIdx, ]
      teamIdx <- nonCoreCut$team == teamInt
      nonCoreOrdinalRank <- nonCoreCut[teamIdx, ]
      rankIdxs <- abs(nonCoreOrdinalRank$rating_day_num - dateInt) %in% min(abs(nonCoreOrdinalRank$rating_day_num - dateInt)) 
      nonCoreOrdinalRank <- nonCoreOrdinalRank[rankIdxs, ]
      namesOR <- nonCoreOrdinalRank$sys_name
      nonCoreOrdinalRank <- nonCoreOrdinalRank$orank 
      if(length(nonCoreOrdinalRank) < lenOR){
        names(nonCoreOrdinalRank) <- namesOR
        ordinalRankNew <- rep(NA, lenOR)
        for(ii in 1:length(nonCoreOrdinalRank)){
          ordinalRankNew[originalNamesOR %in% names(nonCoreOrdinalRank[ii])] <- nonCoreOrdinalRank[ii]
        }
        nonCoreOrdinalRank <- ordinalRankNew
        names(nonCoreOrdinalRank) <- originalNamesOR
      }else{
        names(nonCoreOrdinalRank) <- namesOR
      }
    }
    if(length(nonCoreOrdinalRank) > lenOR){
      return(rep(NA, lenOR))
    }else{
      return(nonCoreOrdinalRank)
    }
  }
    
  nonCoreRankings <- matrix(rep(0, length(seasonVector) * (length(unique(nonCore$sys_name)))), 
                         nrow = length(seasonVector), ncol = (length(unique(nonCore$sys_name))))
  for(i in 1:length(seasonVector)){
    nonCoreRankings[i, ] <- nonCorerankingExtract(teams1[i], seasonVector[i], dateVec[i])    
  }
  nonCoreRankings <- as.data.frame(nonCoreRankings)
  names(nonCoreRankings) <- sort(unique(nonCore$sys_name))
  
  nonCoreRankings2 <- matrix(rep(0, length(seasonVector) * (length(unique(nonCore$sys_name)))), 
                            nrow = length(seasonVector), ncol = (length(unique(nonCore$sys_name))))
  for(i in 1:length(seasonVector)){
    nonCoreRankings2[i, ] <- nonCorerankingExtract(teams1[i], seasonVector[i], dateVec[i])    
  }
  nonCoreRankings2 <- as.data.frame(nonCoreRankings2)
  names(nonCoreRankings2) <- sapply(sort(unique(nonCore$sys_name)), anonFun <-function(string){
    return(paste0(string, 2))
  })    

  #Seeds extractor
  extractSeeds <- function(teamVector, seasonVector){
    Seeds <- rep(0, nrow(tourneyResults))
    for(i in 1:nrow(tourneyResults)){
      Seeds[i] <- tourneySeeds$seed[tourneySeeds$team == teamVector[i] & tourneySeeds$season == seasonVector[i]]    
    }
    Seeds <- as.numeric(gsub('[A-Za-z]', '', Seeds))  
    return(Seeds)
  }
  
  seedsTeam1 <- extractSeeds(teams1, seasonVector)
  seedsTeam2 <- extractSeeds(teams2, seasonVector)
  
  seedBenchmark <- 0.50 + 0.03 * (seedsTeam1 - seedsTeam2)
  
  
  
  return(cbind(seedsTeam1, seedsTeam2, seedBenchmark, coreRankings, coreRankings2, nonCoreRankings1, nonCoreRankings2))
}