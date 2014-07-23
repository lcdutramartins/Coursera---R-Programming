rankall <- function(outcome, num = "best")
{
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia")
  {
    stop("invalid outcome")
  }
  if (!is.numeric(num) && num != "best" && num != "worst")
  {
    stop("invalid num")
  }

  stateList <- data$State
  stateList <- unique(stateList)[order(unique(stateList))]
  hospitalResult <- stateList
  i <- 1  
  for (state in stateList)
  {
    ## Return hospital name in that state with the given rank 30-day death rate
    dataColumn <- if(outcome == "heart attack") { 11 } else if(outcome == "heart failure") { 17 } else { 23 }
    
    indexes <- data$State == state & !is.na(as.numeric(data[[dataColumn]]))
    
    hospitalRank <- order(data$Hospital.Name[indexes])
    valueRank <- order(as.numeric(data[[dataColumn]][indexes][hospitalRank]))
    
    calcNum <- if (is.numeric(num)) { num } else if (num == "best") { 1 } else if (num == "worst") { length(valueRank) } else { NA }    
    rankHospital <- if (is.na(calcNum)) { NA } else { data$Hospital.Name[indexes][hospitalRank][valueRank][calcNum] }
    
    hospitalResult[i] <- rankHospital
    i <- i + 1
  }   

  ## Return a data frame with the hospital names and the (abbreviated) state names
  results <- data.frame(hospital = hospitalResult, state = stateList)
  return(results)
}