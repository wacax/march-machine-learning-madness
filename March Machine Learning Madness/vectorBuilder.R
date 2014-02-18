vectorBuilder <- function(home, visitor, season){
  
  solutionString <- paste(home, visitor, season, sep = '_')
  
  homeSeed <- get(season, envir = seeds.env)
  homeSeed <- homeSeed$team
  visitorSeed <- get(season, envir = seeds.env)
  visitorSeed <- visitorSeed$team
  
  return(list(solutionString))
  
}