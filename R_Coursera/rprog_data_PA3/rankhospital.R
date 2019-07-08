# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name.

# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
# the exact message "invalid outcome".

rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        sub_data <- outcome_data[outcome_data[[ "State" ]] == state, c(outcome_char, "Hospital.Name")]
        ordered_subdata <- sub_data[order(as.numeric(as.character(sub_data[[outcome_char]])), sub_data[["Hospital.Name"]]), ]
        ordered_subdata <- ordered_subdata[ordered_subdata[[outcome_char]] != 'Not Available', ]
        
        # print(ordered_subdata)
        
        if (num > nrow(ordered_subdata)){
                NA
        }
        
        if (num == "best"){
                ordered_subdata[1, "Hospital.Name"] 
        }
        else if (num == "worst"){
                ordered_subdata[nrow(ordered_subdata), "Hospital.Name"] 
        }
        else{
                ordered_subdata[num, "Hospital.Name"]
        }
        
}