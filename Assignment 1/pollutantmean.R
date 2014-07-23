pollutantmean <- function(directory, pollutant, id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
  
  tabResults <- data.frame(Date = factor(), sulfate=numeric(), nitrate=numeric(), ID=integer())
  classes <- sapply(tabResults, class)
  for (i in id)
  {
    filePath <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")
    tabPartial <- read.csv(filePath, colClasses = classes)
    tabResults <- rbind(tabResults, tabPartial)
  }
  
  meanResult <- mean(tabResults[[pollutant]][!is.na(tabResults[[pollutant]])])
  
  return(meanResult)
}