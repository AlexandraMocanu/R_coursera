## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions for a matrix. These
##      functions set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #init
        set <- function(y) {
                x <<- y
                m <<- NULL
        } # set the value of matrix 
        get <- function() x  # get the value of matrix 
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve receives a list created with makeCacheMatrix and returns the lists inverse matrix from
##      the cache if it was calculated before, or it also sets it otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

## USE CASE:
# > matrix_c <- makeCacheMatrix()
# > b = matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=2)
# > matrix_c$set(b)
# > matrix_c$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cachesolve(matrix_c)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cachesolve(matrix_c)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
