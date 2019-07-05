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

cachesolve <- function(x, ...) {
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