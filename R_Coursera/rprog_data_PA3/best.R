# Write a function called best that take two arguments: the 2-character abbreviated name of 
# a state and an outcome name. The function reads the outcome-of-care-measures.csv file and
# returns a character vector with the name of the hospital that has the best (i.e. lowest) 
# 30-day mortality for the specified outcome in that state. The hospital name is the name 
# provided in the Hospital.Name variable. The outcomes can be one of "heart attack", 
# "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome 
# should be excluded from the set of hospitals when deciding the 
# 
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital 
# names should be sorted in alphabetical order and the first hospital in that set should be 
# chosen (i.e. if hospitals "b", "c", and "f" are tied for best, then hospital "b" should be 
# returned).

# The function should check the validity of its arguments. If an invalid state value is passed 
# to best, the function should throw an error via the stop function with the exact message 
# "invalid state". If an invalid outcome value is passed to best, the function should throw an 
# error via the stop function with the exact message "invalid outcome".

best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcomes_names <- data.frame(in_n = c("heart attack", "heart failure", "pneumonia"), 
                               out_n = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  
  ## Check that state and outcome are valid
  if (! is.element(state, outcome_data$State)){
    stop("invalid state")
  }
  if (! is.element(outcome, outcomes)){
    stop("invalid outcome")
  }
  outcome_col <- outcomes_names[which(outcomes_names$in_n == outcome), ]
  outcome_char <- as.character(outcome_col[1, "out_n"])
  
  ## Return hospital name in that state with lowest 30-day death rate
  mins <- tapply(outcome_data[[outcome_char]], outcome_data$State, min, na.rm = TRUE)
  min_value <- mins[state]
  min_hospital <- outcome_data[which((outcome_data$State == state) & (outcome_data[[outcome_char]] == min_value)), ]
  min_hospital <- sort(min_hospital[["Hospital.Name"]])
  # min_hospital <- min_hospital[1, "Hospital.Name"]
  min_hospital[1]
}