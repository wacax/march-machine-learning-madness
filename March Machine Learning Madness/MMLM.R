#March Machine Learning Madness
#Pipeline devel
#Ver 0.3

#########################
#Init
rm(list=ls(all=TRUE))

#Load/install libraries
install.packages("foreign")
install.packages("gbm")
install.packages("cvTools")
install.packages("robustbase")
install.packages("ISLR")
install.packages("boot")
require("foreign")
require("gbm")
require("cvTools")
require("robustbase")
require("ISLR")
require("boot")

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
seasonResults[is.na(seasonResults$numot)] <- 0
seasonResults['numot'] <- as.factor(seasonResults$numot)

tourneyResults['season'] <- as.factor(tourneyResults$season)
tourneyResults['daynum'] <- as.factor(tourneyResults$daynum)
tourneyResults[is.na(tourneyResults$numot)] <- 0
numot <- as.factor(tourneyResults$numot)
wloc <- as.factor(rep('N', nrow(tourneyResults)))
tourneyResults <- tourneyResults[,-7]
tourneyResults <- cbind(tourneyResults, wloc, numot)

#More data munging
#Data shuffling
tourneyShuffleIndexes <- as.logical(sample(c(rep(1, floor(nrow(tourneyResults)/2)), rep(0, floor(nrow(tourneyResults)/2)), nrow(tourneyResults))))
seasonShuffleIndexes <- as.logical(sample(c(rep(1, floor(nrow(seasonResults)/2)), rep(0, ceiling(nrow(seasonResults)/2)), nrow(seasonResults))))
tourneyShuffleIndexes <- tourneyShuffleIndexes[1:length(tourneyShuffleIndexes) - 1]
seasonShuffleIndexes <- seasonShuffleIndexes[1:length(seasonShuffleIndexes) - 1]

#encode "y" in terms of probability where "1" means win and "0" defeat
yTourney <- as.numeric(tourneyShuffleIndexes)
ySeason <- as.numeric(seasonShuffleIndexes)
tourneyResults['y'] <- yTourney
seasonResults['y'] <- ySeason

#Season Results Shuffle
winTeams <- rep(0, length(seasonShuffleIndexes))
winScores <- rep(0, length(seasonShuffleIndexes))
LoseTeams <- rep(0, length(seasonShuffleIndexes))
LoseScores <- rep(0, length(seasonShuffleIndexes))
winTeams[seasonShuffleIndexes] <- seasonResults$wteam[seasonShuffleIndexes]
winTeams[!seasonShuffleIndexes] <- seasonResults$lteam[!seasonShuffleIndexes]
winScores[seasonShuffleIndexes] <- seasonResults$wscore[seasonShuffleIndexes]
winScores[!seasonShuffleIndexes] <- seasonResults$lscore[!seasonShuffleIndexes]
LoseTeams[!seasonShuffleIndexes] <- seasonResults$wteam[!seasonShuffleIndexes]
LoseTeams[seasonShuffleIndexes] <- seasonResults$lteam[seasonShuffleIndexes]
LoseScores[!seasonShuffleIndexes] <- seasonResults$wscore[!seasonShuffleIndexes]
LoseScores[seasonShuffleIndexes] <- seasonResults$lscore[seasonShuffleIndexes]
seasonResults$wteam <- winTeams
seasonResults$wscore <- winScores
seasonResults$lteam <- LoseTeams
seasonResults$lscore <- LoseScores

#Tourney Results Shuffle
winTeams <- rep(0, length(tourneyShuffleIndexes))
winScores <- rep(0, length(tourneyShuffleIndexes))
LoseTeams <- rep(0, length(tourneyShuffleIndexes))
LoseScores <- rep(0, length(tourneyShuffleIndexes))
winTeams[tourneyShuffleIndexes] <- tourneyResults$wteam[tourneyShuffleIndexes]
winTeams[!tourneyShuffleIndexes] <- tourneyResults$lteam[!tourneyShuffleIndexes]
winScores[tourneyShuffleIndexes] <- tourneyResults$wscore[tourneyShuffleIndexes]
winScores[!tourneyShuffleIndexes] <- tourneyResults$lscore[!tourneyShuffleIndexes]
LoseTeams[!tourneyShuffleIndexes] <- tourneyResults$wteam[!tourneyShuffleIndexes]
LoseTeams[tourneyShuffleIndexes] <- tourneyResults$lteam[tourneyShuffleIndexes]
LoseScores[!tourneyShuffleIndexes] <- tourneyResults$wscore[!tourneyShuffleIndexes]
LoseScores[tourneyShuffleIndexes] <- tourneyResults$lscore[tourneyShuffleIndexes]
tourneyResults$wteam <- winTeams
tourneyResults$wscore <- winScores
tourneyResults$lteam <- LoseTeams
tourneyResults$lscore <- LoseScores

#######################################################
#Environments
source(paste0(workingDirectory, 'assignToEnvironment.R'))

#Assign names to new environments
seasons.env <- new.env()
teams.env <- new.env()
seeds.env <- new.env()
slots.env <- new.env()
historic.env <- new.env()

assignToEnvironment(seasons$season, seasons[, -1], seasons.env)
assignToEnvironment(teams$id, teams[, -1], teams.env) #fix this
assignToEnvironment(as.character(tourneySeeds$season), tourneySeeds[, -1], seeds.env)
assignToEnvironment(tourneySlots$season, tourneySlots[, -1], slots.env)
assignToEnvironment(as.character(SportsReferenceData$id), SportsReferenceData[, c(-1, -2, -3)], historic.env)

#historic data of teams (Train)
anonFun <- function(keyWin, keyLose, environment, vectorZeros, SportsReferenceData){
  if(sum(SportsReferenceData$id == keyWin) != 0 & sum(SportsReferenceData$id == keyLose) != 0){
    vectorWin <- get(as.character(keyWin), envir = environment)
    vectorLose <- get(as.character(keyLose), envir = environment)
    historicVector <- as.numeric(cbind(vectorWin, vectorLose))
  }else{
    historicVector <- c(vectorZeros, vectorZeros)
  }
}
#season historic
vectorZeros <- rep(0, 15)
seasonHistoric <- matrix(rep(0, nrow(seasonResults) * 30), nrow=nrow(seasonResults), ncol=30) 
for(i in 1:nrow(seasonResults)){
  seasonHistoric[i, ] <- anonFun(seasonResults$wteam[i], seasonResults$lteam[i], historic.env, vectorZeros, SportsReferenceData)
}
#season seeds (dummy)
winSeeds <- rep(17, nrow(seasonResults))
loseSeeds <- rep(17, nrow(seasonResults))

seasonResults <- cbind(seasonResults, winSeeds, loseSeeds, seasonHistoric)
#colnames <- names(SportsReferenceData); colnames <- colnames[c(-1, -2, -3)]
colnames <- c("From","To","Yrs","G","W","L","W.L.","SRS","SOS","AP","CREG","CTRN","NCAA","FF","NC","From2","To2","Yrs2","G2","W2","L2","W.L.2","SRS2","SOS2","AP2","CREG2","CTRN2","NCAA2","FF2","NC2")
colnames <- c(names(seasonResults[1:11]), colnames)
names(seasonResults) <- colnames

#tourney historic
vectorZeros <- rep(0, 15)
tournamentHistoric <- matrix(rep(0, nrow(tourneyResults) * 30), nrow=nrow(tourneyResults), ncol=30) 
for(i in 1:nrow(tourneyResults)){
  tournamentHistoric[i, ] <- anonFun(tourneyResults$wteam[i], tourneyResults$lteam[i], historic.env, vectorZeros, SportsReferenceData)
}

#tourney Seeds
winSeeds <- rep(0, nrow(tourneyResults))
for(i in 1:nrow(tourneyResults)){
  winSeeds[i] <- tourneySeeds$seed[tourneySeeds$team == tourneyResults$wteam[i] & tourneySeeds$season == tourneyResults$season[i]]    
}
winSeeds <- as.numeric(gsub('[A-Za-z]', '', winSeeds))

loseSeeds <- rep(0, nrow(tourneyResults))
for(i in 1:nrow(tourneyResults)){
  loseSeeds[i] <- tourneySeeds$seed[tourneySeeds$team == tourneyResults$lteam[i] & tourneySeeds$season == tourneyResults$season[i]]    
}
loseSeeds <- as.numeric(gsub('[A-Za-z]', '', loseSeeds))

tourneyResults <- cbind(tourneyResults, winSeeds, loseSeeds, tournamentHistoric)
#colnames <- names(SportsReferenceData); colnames <- colnames[c(-1, -2, -3)]
colnames <- c("From","To","Yrs","G","W","L","W.L.","SRS","SOS","AP","CREG","CTRN","NCAA","FF","NC","From2","To2","Yrs2","G2","W2","L2","W.L.2","SRS2","SOS2","AP2","CREG2","CTRN2","NCAA2","FF2","NC2")
colnames <- c(names(tourneyResults[1:11]), colnames)
names(tourneyResults) <- colnames

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

########################################################
#Training
tourneyResults <- rbind(seasonResults, tourneyResults)
#get fun also works when extracts data from non-environments i.e dataframes
# set up function call
source(paste0(workingDirectory, 'predictionsVectorExtractor.R'))
#home made validation
trainIdxs <- sort(sample(1:nrow(tourneyResults), floor(nrow(tourneyResults)*0.7)))
testIdxs <- !(1:nrow(tourneyResults) %in%  trainIdxs)
trainIdxs <- (1:nrow(tourneyResults) %in%  trainIdxs)
tourneyResultsTest <- tourneyResults[testIdxs, ]
tourneyResults <- tourneyResults[trainIdxs, ]

#tourney model
glm.fit = glm(formula = y ~ wloc + season + winSeeds + loseSeeds + G + G2 + W + W2 + L + L2 + W.L. + W.L.2 + SRS + SRS2 + SOS + SOS2 + AP + AP2 + CREG + CREG2  + CTRN + CTRN2 + NCAA + NCAA2 + FF + FF2 + NC + NC2 , data = tourneyResults)
gbm.fit = gbm(formula = y ~ wloc + season + winSeeds + loseSeeds + G + G2 + W + W2 + L + L2 + W.L. + W.L.2 + SRS + SRS2 + SOS + SOS2 + AP + AP2 + CREG + CREG2  + CTRN + CTRN2 + NCAA + NCAA2 + FF + FF2 + NC + NC2 , data = tourneyResults, n.trees = 10000)
#cv.glm(tourneyResults, glm.fit) #this is the generic function
print(glm.fit)
print(gbm.fit)
predictionFromModel <- predict(glm.fit, tourneyResultsTest)
predictionFromModel[predictionFromModel <= 0] <- 0.0000001
predictionFromModel[predictionFromModel >= 1] <- 0.9999999
predictionFromGBM <- predict(gbm.fit, tourneyResultsTest, 10000)
predictionFromGBM[predictionFromGBM <= 0] <- 0.0000001
predictionFromGBM[predictionFromGBM >= 1] <- 0.9999999

#Let's write a simple function to use formula
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
# Now we try it out
loocv(glm.fit)

cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit = glm(formula = y ~ season + winSeeds + loseSeeds + G + G2 + W + W2 + L + L2 + W.L. + W.L.2 + SRS + SRS2 + SOS + SOS2 + AP + AP2 + CREG + CREG2  + CTRN + CTRN2 + NCAA + NCAA2 + FF + FF2 + NC + NC2 , data = tourneyResults)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

#perform 10 fold X-validation using cvTools
cv.error10=rep(0,5)
for(d in degree){
  glm.fit = glm(formula = y ~ season + winSeeds + loseSeeds + G + G2 + W + W2 + L + L2 + W.L. + W.L.2 + SRS + SRS2 + SOS + SOS2 + AP + AP2 + CREG + CREG2  + CTRN + CTRN2 + NCAA + NCAA2 + FF + FF2 + NC + NC2 , data = tourneyResults)
  cv.error10[d] = cv.glm(tourneyResults, glm.fit, K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
#dummyModel <- cvFit(dummyModel, data = seasonResults, y = seasonResults$wscore,
#                    K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

predicted <- predict(dummyModel, predictionsVectorExtractor(predictionGames[,1]))

########################################################
#Evaluation
#Submissions are scored on the log loss
source(paste0(workingDirectory, 'customLogLoss.R'))
require("Metrics")
names(predictionFromModel) <- NULL
predictionFromModel[is.na(predictionFromModel)] <- 0
scoreGLM <- logLoss(tourneyResultsTest$y, predictionFromModel)
print(scoreGLM)
scoreGLM2 <- customLogLoss(tourneyResultsTest$y, predictionFromModel)
print(scoreGLM2)
scoreGBM <- logLoss(tourneyResultsTest$y, predictionFromGBM)
print(scoreGBM)
scoreGBM2 <- customLogLoss(tourneyResultsTest$y, predictionFromGBM)
print(scoreGBM2)