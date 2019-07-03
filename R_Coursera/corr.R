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

# PART 3 - 'corr'

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the 
# correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
# (on all variables) is greater than the threshold. The function should return a vector of correlations for the 
# monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function 
# should return a numeric vector of length 0. 

corr <- function(directory, threshold = 0){
        
        correlations <- numeric()
        id = 1:332
        
        for(i in id){
                # formatC(30, width=3, flag="0") --> ex. format as 008
                path <- file.path(directory, paste(formatC(i, width=3, flag="0"), ".csv", sep = ""))
                csv_file_data <- read.csv(path)
                
                complete_cases <- csv_file_data[!is.na(csv_file_data$sulfate) & !is.na(csv_file_data$nitrate), ]
                # print(nrow(complete_cases))
                if(nrow(complete_cases) > threshold){
                        complete_case_sulfate <- complete_cases$sulfate
                        complete_case_nitrate <- complete_cases$nitrate
                        # print(complete_case_sulfate)
                        # print(complete_case_nitrate)

                        correlations <- c(cor(complete_case_nitrate, complete_case_sulfate), correlations)
                }
                
        }
        
        correlations
}