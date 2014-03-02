#March Machine Learning Madness
#Pipeline devel
#Ver 0.2

#########################
#Init
rm(list=ls(all=TRUE))

#Load/install libraries
install.packages("foreign")
install.packages("gbm")
install.packages("cvTools")
install.packages("robustbase")
require("foreign")
require("gbm")
require("cvTools")
require("robustbase")

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
SportsReferenceData <- read.csv(paste0(dataDirectory, 'SportsReferenceData.csv'), header = TRUE, stringsAsFactors = FALSE)

predictionGames <- read.csv(paste0(dataDirectory, 'sample_submission.csv'), header = TRUE, stringsAsFactors = FALSE)

#Glosary
SportsReferenceDataGlossary <- read.csv(paste0(dataDirectory, 'SportsReferenceDataGlossary.csv'), header = TRUE, stringsAsFactors = FALSE)

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

tourneyResults['season'] <- as.factor(tourneyResults$season)
tourneyResults['daynum'] <- as.factor(tourneyResults$daynum)
tourneyResults['wloc'] <- as.factor(rep('N', nrow(tourneyResults)))
tourneyResults['numot'] <- as.factor(tourneyResults$numot)

#######################################################
#Visualizations
#Regular Season Results Graphs
identityMatrix <- diag(max(as.numeric(seasonResults[,1])))
seasonsDense <- t(sapply(as.numeric(seasonResults[,1]), anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))
identityMatrix <- diag(max(as.numeric(seasonResults[,7])))
wLocDense <- t(sapply(as.numeric(seasonResults[,7]), anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))

covarianceMatrix <- cor(cbind(seasonResults['wscore'], seasonResults['lscore'],  as.matrix(wLocDense)))
image(covarianceMatrix, xlab = 'WinScore,         LoseScore,          Home,          Away,          Neutral')

#Regular Season Results Graphs + Tournament Results Graphs
identityMatrix <- diag(max(as.numeric(tourneyResults[,1])))
seasonsDenseT <- t(sapply(as.numeric(tourneyResults[,1]), anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))
identityMatrix <- diag(max(as.numeric(tourneyResults[,8])))
wLocDenseT <- t(sapply(as.numeric(tourneyResults[,8]), anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))

neutralMatrix <- matrix(nrow = nrow(tourneyResults), ncol = 3)
neutralMatrix[,1:2] <- 0
neutralMatrix[,3] <- 1

covarianceMatrix <- cor(cbind(c(seasonResults['wscore'], tourneyResults['wscore']), c(seasonResults['wscore'], tourneyResults['lscore']),  rbind(as.matrix(wLocDense), neutralMatrix)))
image(covarianceMatrix, xlab = 'WinScore,         LoseScore,          Home,          Away,          Neutral')

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

hist(tourneyResults$wscore, xlab = 'Score', main = 'Winning scores')
hist(tourneyResults$lscore, xlab = 'Score', main = 'Losing scores')

#read .dta file (utility function). It must be in binary format
importFromStata <- function(fileName){
  dataTest <- read.dta(paste0(dataDirectory, missing.type = TRUE))
}

#######################################################
#Environments
source(paste0(workingDirectory, 'assignToEnvironment.R'))

#Assign names to new environments
seasons.env <- new.env()
teams.env <- new.env()
seeds.env <- new.env()
slots.env <- new.env()

assignToEnvironment(seasons$season, seasons[, -1], seasons.env)
assignToEnvironment(teams$id, teams[, -1], teams.env) #fix this
assignToEnvironment(tourneySeeds$season, tourneySeeds[, -1], seeds.env)
assignToEnvironment(tourneySlots$season, tourneySlots[, -1], slots.env)

########################################################
#Training
#get fun also works when extracts data from non-environments i.e dataframes
# set up function call
dummyModel <- gbm(wscore ~ season + wloc,
                    data = seasonResults)

#perform 5 fold X-validation using cvTools
dummyModel <- cvFit(dummyModel, data = seasonResults, y = seasonResults$wscore,
                    K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

predicted <- predict(dummyModel, *dataFromVetorBuilder*)

########################################################
#Evaluation
#Submissions are scored on the log loss
require("Metrics")
score <- logLoss(actual, predicted)
print(score)
