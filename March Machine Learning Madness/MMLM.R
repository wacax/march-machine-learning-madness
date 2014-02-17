#March Machine Learning Madness
#Pipeline devel
#Ver 0.2

#########################
#Init
rm(list=ls(all=TRUE))

#Load/install libraries
install.packages("foreign")
install.packages("gbm")
install.packages("robustbase")
install.packages("cvTools")
require("foreign")
require("gbm")
require("robustbase")
require("cvTools")

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
seasonResults['wloc'] <- as.factor(seasonResults$wloc)
seasonResults['numot'] <- as.factor(seasonResults$numot)

#######################################################
#Visualizations
covarianceMatrix <- cov2cor(seasonResults['season', 'wscore', 'lscore', 'wloc', 'numot'])

#histograms
hist(as.numeric(seasonResults$season))
hist(seasonResults$wscore, xlab = 'Score', main = 'Winning scores')
hist(seasonResults$lscore, xlab = 'Score', main = 'Losing scores')
hist(seasonResults$wscore[seasonResults$wloc == 'H'], xlab = 'Score', main = 'Winning scores, home')
hist(seasonResults$lscore[seasonResults$wloc == 'H'], xlab = 'Score', main = 'Losing scores, home')
hist(seasonResults$wscore[seasonResults$wloc == 'A'], xlab = 'Score', main = 'Winning scores, away')
hist(seasonResults$lscore[seasonResults$wloc == 'A'], xlab = 'Score', main = 'Losing scores, away')
hist(seasonResults$wscore[seasonResults$wloc == 'N'], xlab = 'Score', main = 'Winning scores, neutral')
hist(seasonResults$lscore[seasonResults$wloc == 'N'], xlab = 'Score', main = 'Losing scores, neutral')

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
#######################################################
#5 fold X-validation


########################################################
#Training
#get fun also works when extracts data from non-environments i.e dataframes
dummyModel <- gbm(wscore ~ season + wloc + daynum,
                    data = seasonResults)

########################################################
#Evaluation
#Submissions are scored on the log loss
require("Metrics")
score <- logLoss(actual, predicted)
print(score)
