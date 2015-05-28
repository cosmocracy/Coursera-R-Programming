#setwd('C:/TDE/GitHub/Coursera-R-Programming/ProgrammingAssignment3-HospitalQuality/Data')
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
#hist(outcome[, 11])

rankhospital <- function(state, outcome, num = "best") {
  # Read the outcome data from CSV
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Derive the valid inputs based upon the shape/content of the data file
  valid_states <- unique(outcomes[,"State"])
  outcome_mapping <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                          "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                          "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  valid_outcomes <- names(outcome_mapping)

  # Convert the textual numeric data to actual numeric data
  for(column in outcome_mapping[]) {
    outcomes[,column] <- suppressWarnings(as.numeric(outcomes[,column]))
  }
  
  # Validate Inputs
  
  if(! state %in% valid_states) {
    stop("invalid state")
  }
  
  if(! outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # Narrow the data to the hospitals in the state of interest  
  state_outcomes <- outcomes[which(outcomes$State == state),]
  
  # Determine the actual column name of our desired column
  target_outcome_column_name = outcome_mapping[[outcome]]
  # Sort by the desired measure (Assumption is that lower rates are better)
  state_outcomes <- state_outcomes[order(state_outcomes[,target_outcome_column_name], state_outcomes[,"Hospital.Name"]),]
  # Eliminate rows that aren't numeric for the desired measure
  state_outcomes <- state_outcomes[which(!is.na(state_outcomes[,target_outcome_column_name])),]
  
  if(nrow(state_outcomes) == 0) {
    stop("no data available for state")
  } else if(num == "best") {
    record <- head(state_outcomes,1)
  } else if(num == "worst") {
    record <- tail(state_outcomes,1)
  } else if(is.numeric(num)) {
    index <- as.numeric(num)
    if(index > nrow(state_outcomes)) {
      return(NA)
    } else {
      record <- state_outcomes[index,]
    }
  } else {
    stop("encountered non-numeric 'num' parameter other than 'best'/'worst'")
  }
  return(record$"Hospital.Name")
}
