complete <- function(directory, id = 1:332) 
{
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form below, where 'id' is the monitor ID number and 'nobs' is the number of complete cases 
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  idResults <- id
  nobsResults <- id
  
  tabData <- data.frame(Date = factor(), sulfate=numeric(), nitrate=numeric(), ID=integer())
  dataClasses <- sapply(tabData, class)
  for (i in seq_along(id))
  {
    filePath <- paste(directory, "/", formatC(id[i], width = 3, flag = "0"), ".csv", sep = "")
    tabData <- read.csv(filePath, colClasses = dataClasses)
    
    idResults[i] <- id[i]
    nobsResults[i] <- sum(complete.cases(tabData))
  }

  results <- data.frame(id=idResults, nobs=nobsResults)
  
  return(results)
}