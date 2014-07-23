corr <- function(directory, threshold = 0) 
{
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all variables) required to compute the correlation between nitrate and sulfate
  
  ## Return a numeric vector of correlations

  results <- numeric(0)
  index <- 1
  
  tabEmpty <- data.frame(Date = factor(), sulfate=numeric(), nitrate=numeric(), ID=integer())
  classes <- sapply(tabEmpty, class)
  
  for (id in 1:332)
  {
    filePath <- paste(directory, "/", formatC(id, width = 3, flag = "0"), ".csv", sep = "")
    tabData <- read.csv(filePath, colClasses = classes)
    filter <- complete.cases(tabData)
    
    if (sum(filter) > threshold)
    {
      results[index] <- cor(tabData[["sulfate"]][filter], tabData[["nitrate"]][filter])
      index <- index + 1
    }
  }
  
  return(results)
}