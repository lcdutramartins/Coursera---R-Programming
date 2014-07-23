best <- function(state, outcome)
{
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State))
  {
    stop("invalid state")
  }
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia")
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  dataColumn <- if(outcome == "heart attack") { 11 } else if(outcome == "heart failure") { 17 } else { 23 }
  
  stateIndexes <- data$State == state
  casesIndexes <- !is.na(as.numeric(data[[dataColumn]]))

  bestRate <- min(as.numeric(data[[dataColumn]][stateIndexes & casesIndexes]))
  ratesIndexes <- as.numeric(data[[dataColumn]]) == bestRate

  bestHospitals <- data$Hospital.Name[stateIndexes & casesIndexes & ratesIndexes]

  untieOrder <- order(bestHospitals)
  
  bestHospitals[untieOrder][1]
}