# PROGRAMMING ASSIGNMENTS

# The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine 
# matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single 
# monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 
# 200 is contained in the file "200.csv". Each file contains three variables:
#         
#         Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#         sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#         nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)
# 
# For this programming assignment you will need to unzip this file and create the directory 'specdata'. 
# Once you have unzipped the zip file, do not make any modifications to the files in the 'specdata' directory. 
# each file you'll notice that there are many days where either sulfate or nitrate (or both) are missing 
# (coded as NA). This is common with air pollution monitoring data in the United States.

# PART 2 - 'complete'

# Write a function that reads a directory full of files and reports the number of completely observed cases 
# in each data file. The function should return a data frame where the first column is the name of the file 
# and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
        complete_cases <- data.frame(id = numeric(), nobs = numeric())
        k <- 1
        for(i in id){
                # formatC(30, width=3, flag="0") --> ex. format as 008
                csv_file_data <- read.csv(paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep = ""))
                complete_cases[k, "id"] <- i
                complete_cases[k, "nobs"] <- length(csv_file_data$Date[which(!is.na(csv_file_data$sulfate) & !is.na(csv_file_data$nitrate))])
                k <- k+1
        }
        complete_cases
}