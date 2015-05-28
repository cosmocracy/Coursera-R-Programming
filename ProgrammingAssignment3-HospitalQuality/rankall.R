#setwd('C:/TDE/GitHub/Coursera-R-Programming/ProgrammingAssignment3-HospitalQuality/Data')
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
#hist(outcome[, 11])

rankall <- function(outcome, num = "best") {
  # Read the outcome data from CSV
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Derive the valid inputs based upon the shape/content of the data file
  valid_states <- unique(outcomes[,"State"])
  valid_states <- sort(valid_states)
  outcome_mapping <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                          "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                          "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  valid_outcomes <- names(outcome_mapping)

  # Convert the textual numeric data to actual numeric data
  for(column in outcome_mapping[]) {
    outcomes[,column] <- suppressWarnings(as.numeric(outcomes[,column]))
  }
  
  # Validate Inputs
  
  if(! outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  result <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  for(state in valid_states) {
    # Narrow the data to the hospitals in the state of interest  
    state_outcomes <- outcomes[which(outcomes$State == state),]
    # Determine the actual column name of our desired column
    target_outcome_column_name = outcome_mapping[[outcome]]
    # Sort by the desired measure (Assumption is that lower rates are better)
    state_outcomes <- state_outcomes[order(state_outcomes[,target_outcome_column_name], state_outcomes[,"Hospital.Name"]),]
    # Eliminate rows that aren't numeric for the desired measure
    state_outcomes <- state_outcomes[which(!is.na(state_outcomes[,target_outcome_column_name])),]
    
    hospital <- NA
    if(nrow(state_outcomes) == 0) {
      hospital <- NA
      result <- rbind(result, data.frame(hospital = hospital, state = state))
    } else if(num == "best") {
      record <- head(state_outcomes,1)
      hospital <- record$"Hospital.Name"
      result <- rbind(result, data.frame(hospital = hospital, state = state))
    } else if(num == "worst") {
      record <- tail(state_outcomes,1)
      hospital <- record$"Hospital.Name"
      result <- rbind(result, data.frame(hospital = hospital, state = state))
    } else if(is.numeric(num)) {
      index <- as.numeric(num)
      if(index > nrow(state_outcomes)) {
        hospital <- NA
        result <- rbind(result, data.frame(hospital = hospital, state = state))
      } else {
        record <- state_outcomes[index,]
        hospital <- record$"Hospital.Name"
        result <- rbind(result, data.frame(hospital = hospital, state = state))
      }
    } else {
      stop("encountered non-numeric 'num' parameter other than 'best'/'worst'")
    }
  }
  return(result)
}
