RankingsExtractor <- function(teams1, teams2, seasonVector, date = NULL){
  
  sagarinRanking <- read.csv(paste0(dataDirectory, 'sagp_weekly_ratings.csv'), header = TRUE, stringsAsFactors = FALSE)
  core33 <- read.csv(paste0(dataDirectory, 'ordinal_ranks_core_33.csv'), header = TRUE, stringsAsFactors = FALSE)
  nonCore <- read.csv(paste0(dataDirectory, 'ordinal_ranks_non_core.csv'), header = TRUE, stringsAsFactors = FALSE)
  seasons <- read.csv(paste0(dataDirectory, 'seasons.csv'), header = TRUE, stringsAsFactors = FALSE)
  
  dayZeroSeasons <- sapply(seasons$dayzero, anonFun <- function(string){
    as.Date(string, format = '%m/%d/%Y')
  })
    
  
  if (date == NULL){
    date
  }
}