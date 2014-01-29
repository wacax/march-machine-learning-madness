#March Machine Learning Madness
#What am I doing with my lives
#Ver 0.1

#########################
#Init
rm(list=ls(all=TRUE))
#Set Working Directory
#workingDirectory <- 'D:/Wacax/Repos/March Madness'
workingDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/March Machine Learning Madness/'
setwd(workingDirectory)
#dataDirectory <- 'D:/Wacax/Repos/March Madness/Data/'
dataDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/March Machine Learning Madness/Data/'

###########################
#Input Data
seasonResults <- read.csv(paste0(dataDirectory, 'regular_season_results.csv'), header = TRUE, stringsAsFactors = FALSE)
seasons <- read.csv(paste0(dataDirectory, 'seasons.csv'), header = TRUE, stringsAsFactors = FALSE)
tourneyResults <- read.csv(paste0(dataDirectory, 'tourney_results.csv'), header = TRUE, stringsAsFactors = FALSE)
tourneySlots <- read.csv(paste0(dataDirectory, 'tourney_slots.csv'), header = TRUE, stringsAsFactors = FALSE)
teams <- read.csv(paste0(dataDirectory, 'teams.csv'), header = TRUE, stringsAsFactors = FALSE)
tourneySeeds <- read.csv(paste0(dataDirectory, 'tourney_seeds.csv'), header = TRUE, stringsAsFactors = FALSE)

#load extra Data
#EXTRA DATA - Pointspreads
thePredictionTracker <- 'http://www.thepredictiontracker.com/ncaabbtoday.csv'
download.file(thePredictionTracker, paste0(dataDirectory, 'predictionTracker.csv'), method = 'auto')
predictTracker <- read.csv(paste0(dataDirectory, 'predictionTracker.csv'), header = TRUE, stringsAsFactors = FALSE)

#EXTRA DATA - Sagarin Predictive Ratings
sagarinPredictiveRankings <- 'https://www.kaggle.com/blobs/download/forum-message-attachment-files/990/sagp_weekly_ratings.csv'
download.file(sagarinPredictiveRankings, paste0(dataDirectory, 'sagarinRankings.csv'), method = 'curl')
sagarinRankings <- read.csv(paste0(dataDirectory, 'sagarinRankings.csv'), header = TRUE, stringsAsFactors = FALSE)

#EXTRA DATA - Ordinal Ranks from Kenneth Massey core 33
ordinalRankingsKMcore33 <- 'http://www.kaggle.com/blobs/download/forum-message-attachment-files/999/ordinal_ranks_core_33.csv'
download.file(ordinalRankingsKMcore33, paste0(dataDirectory, 'core33.csv'), method = 'auto')
sagarinRankingsCore33 <- read.csv(paste0(dataDirectory, 'core33.csv'), header = TRUE, stringsAsFactors = FALSE)

#EXTRA DATA - Ordinal Ranks from Kenneth Massey NON-core
ordinalRankingsKMNonCore <- 'http://www.kaggle.com/blobs/download/forum-message-attachment-files/1000/ordinal_ranks_non_core.csv'
download.file(ordinalRankingsKMNonCore, paste0(dataDirectory, 'nonCore.csv'), method = 'auto')
sagarinRankingsNonCore <- read.csv(paste0(dataDirectory, 'nonCore.csv'), header = TRUE, stringsAsFactors = FALSE)


#Vizualizations

#Evaluation
#Submissions are scored on the log loss
require("Metrics")
score <- logLoss(actual, predicted)
print(score)