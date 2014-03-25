#hello shiny
library(shiny)
#runExample("01_hello")
#runExample("02_text")
#runExample("03_reactivity")
#runExample("05_sliders")

#App Directory
appDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/March Machine Learning Madness/March Machine Learning Madness/shinyapp/'

#Install package
#install.packages("shiny")

require('shiny')
runApp(appDirectory)

#R -e "shiny::runApp('~/shinyapp')"
runCommand <- paste0("shiny::runApp('", appDirectory, "')")
R -e runCommand