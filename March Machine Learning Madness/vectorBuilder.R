vectorBuilder <- function(home, visitor, season){
  
  solutionString <- paste(home, visitor, season, sep = '_')
  
  homeSeed <- get(season, envir = seeds.env)['team' == home]
  visitorSeed <- get(season, envir = seeds.env)['team' == visitor]
  
  return(list(solutionString))
  
}