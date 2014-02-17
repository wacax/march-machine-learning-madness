#March Machine Learning Madness
#Pipeline devel
#Ver 0.2

#########################
#Init
rm(list=ls(all=TRUE))
library("foreign", lib.loc="/usr/lib/R/library")
library("gbm", lib.loc="/home/wacax/R/x86_64-pc-linux-gnu-library/3.0")

#Set Working Directory
#workingDirectory <- 'D:/Wacax/Repos/March Madness'
workingDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/March Machine Learning Madness/March Machine Learning Madness/'
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

######################################################
#Data munching
seasonResults['season'] <- as.factor(seasonResults$season)
seasonResults['daynum'] <- as.factor(seasonResults$daynum)

#######################################################
#Visualizations

hist(as.numeric(seasonResults$season))
hist(as.numeric(seasonResults$sdaynum), breaks = 20)

importFromStata <- function{
  dataTest <- (paste0(dataDirectory, missing.type = TRUE))
}

#######################################################
#Environments
source(paste0(workingDirectory, 'assignToEnvironment.R'))

#Assign names to new environments
seasons.env <- new.env()
teams.env <- new.env()
seeds.env <- new.env()
slots.env <- new.env()

assignToEnvironment(seasons$season, seasons, seasons.env)
assignToEnvironment(teams$id, teams, teams.env)
assignToEnvironment(tourneySeeds$season, tourneySeeds, seeds.env)
assignToEnvironment(tourneySlots$season, tourneySlots, slots.env)

########################################################
#Training


########################################################
#Evaluation
#Submissions are scored on the log loss
require("Metrics")
score <- logLoss(actual, predicted)
print(score)
