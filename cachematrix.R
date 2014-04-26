## These functions collectively solve and cache for the inverse of a matrix
## Before solving, however, they detect if the solution has been cached
## If it has been cached, the cached value it returned

## makeCacheMatrix.R creates a list of functions that are used by cacheSolve.R

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve.R determines if the inverse is cached and then calculates it if it's not

cacheSolve <- function(x, ...) {
        m <- x$getinv()         ## retrieves the matrix inverse
        if(!is.null(m)) {       ## determines if matrix inverse exists yet
                message("getting cached data")
                return(m)       ## returns matrix inverse and exits function
        }
        data <- x$get()         ## retrieves matrix
        m <- solve(data)        ## solves for matrix inverse
        x$setinv(m)             ## stores solution in m
        m                       ## returns matrix inverse
}
