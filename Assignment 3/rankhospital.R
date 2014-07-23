rankhospital <- function(state, outcome, num = "best")
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
  if (!is.numeric(num) && num != "best" && num != "worst")
  {
    stop("invalid num")
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  dataColumn <- if(outcome == "heart attack") { 11 } else if(outcome == "heart failure") { 17 } else { 23 }
  
  indexes <- data$State == state & !is.na(as.numeric(data[[dataColumn]]))

  hospitalRank <- order(data$Hospital.Name[indexes])
  valueRank <- order(as.numeric(data[[dataColumn]][indexes][hospitalRank]))
  
  result <- if (is.numeric(num)) { num } else if (num == "best") { 1 } else if (num == "worst") { length(valueRank) } else { NA }
  
  rankHospital <- if (is.na(result)) { NA } else { data$Hospital.Name[indexes][hospitalRank][valueRank][result] }
  
  rankHospital
}