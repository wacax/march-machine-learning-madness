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


#Visualizations

#Evaluation
#Submissions are scored on the log loss
require("Metrics")
score <- logLoss(actual, predicted)
print(score)