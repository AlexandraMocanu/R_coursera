# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties

# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message "invalid outcome". The num
# variable can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.


rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomes_names <- data.frame(in_n = c("heart attack", "heart failure", "pneumonia"), 
                                     out_n = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        
        ## Check outcome is valid
        if (! is.element(outcome, outcomes)){
                stop("invalid outcome")
        }
        outcome_col <- outcomes_names[which(outcomes_names$in_n == outcome), ]
        outcome_char <- as.character(outcome_col[1, "out_n"])
        
        ## For each state, find the hospital of the given rank
        sub_data <- outcome_data[, c("State", outcome_char, "Hospital.Name")]
        ordered_subdata <- sub_data[order(sub_data[["State"]], as.numeric(as.character(sub_data[[outcome_char]])), sub_data[["Hospital.Name"]]), ]
        ordered_subdata <- ordered_subdata[ordered_subdata[[outcome_char]] != 'Not Available', ]
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rank_hospital <- list("State" = character(), outcome_char = character(), "Hospital.Name" = character())
        states <- unique(ordered_subdata$State)
        
        mins <- tapply(ordered_subdata[[outcome_char]], ordered_subdata$State, min)
        maxs <- tapply(ordered_subdata[[outcome_char]], ordered_subdata$State, max)
        
        hospitals <- list()
        for (state in states){
                if (num == "best"){
                        min_s <- mins[state]
                        row <- ordered_subdata[which(ordered_subdata[["State"]] == state & ordered_subdata[[outcome_char]] == min_s), c("State", outcome_char, "Hospital.Name")]
                        hospitals <- c(hospitals, as.character(row["Hospital.Name"]))
                }
                else if (num == "worst"){
                        max_s <- maxs[state]
                        row <- ordered_subdata[which(ordered_subdata[["State"]] == state & ordered_subdata[[outcome_char]] == max_s), c("State", outcome_char, "Hospital.Name")]
                        hospitals <- c(hospitals, as.character(row["Hospital.Name"]))
                }
                else if (num > length(unique(ordered_subdata[ordered_subdata[["State"]] == state, "Hospital.Name"]))){
                        rank_hospital <- NA
                        print(num)
                }
                else{
                        state_rows <- ordered_subdata[which(ordered_subdata[["State"]] == state), c("State", outcome_char, "Hospital.Name")]
                        row <- state_rows[num, c("State", outcome_char, "Hospital.Name")]
                        hospitals <- c(hospitals, as.character(row["Hospital.Name"]))
                }
                
                
        }
        # as.data.frame(as.matrix(rank_hospital))
        rank_hospital <- do.call(rbind.data.frame, Map('c', states, hospitals))
        rank_hospital
        
}