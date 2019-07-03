add2 <- function(x, y) {
        x + y
}
# you don't need a return because an R function returns 
# the result of whatever the last expression was !!!

above12 <- function(x) { # this function returns the subset of x with elements
        # greater than 12
        elements_greater_12 <- x > 12
        x[elements_greater_12]
}

above_n <- function(x, n = 12) { # n defaults to 12 in case it is not given
        elements_greater_n <- x > n
        x[elements_greater_n]
}

column_mean <- function(y, removeNAs = TRUE) { # calculates the mean of each column;
        # y=dataframe/matrix
        # remove NAs from calculation
        nc <- ncol(y) # nb of columns
        means <- numeric(nc) # empty init of means vector results
        for (i in 1:nc){
                means[i] <- mean(y[, i], na.rm = removeNAs) # y[, i] = column i, all rows
        }
        
        means
        
}

z <- 4

freef <- function(x, y){
        x + y + z
}

make.power.function <- function(N) { # this function returns another function
        pow <- function(x) {
                x^N
        }
        pow
}


# LEXICAL SCOPING = free variables are searched in the environment in which 
                #   the calling functions were DEFINED
# DYNAMIC SCOPING = free variables are searched in the environment in which 
                #   the calling functions were CALLED
