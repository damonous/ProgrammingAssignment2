## Inverse of a Matrix

##Calculates the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function maintains the inverserve of a matrix to improve performance by storing
## the inverserve in memory instead of recalculation the value each time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    
    if(!is.null(m)) {
        message("Getting cached data...")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
