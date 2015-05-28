#setwd('C:/TDE/GitHub/Coursera-R-Programming/ProgrammingAssignment3-HospitalQuality/Data')
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
#hist(outcome[, 11])

best <- function(state, outcome) {
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
  
  # Sort by the desired measure (Assumption is that lower rates are better)
  target_outcome_column_name = outcome_mapping[[outcome]]
  state_outcomes_sorted <- state_outcomes[order(state_outcomes[,target_outcome_column_name], state_outcomes[,"Hospital.Name"]),]
  first_record <- state_outcomes_sorted[1,]
  # Return the first row (knowing it will have the lowest value and that NAN (etc.) will sort
  # to the bottom in the default collation)
  first_record$"Hospital.Name"
}
