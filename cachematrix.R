# This function caches the inverse of a matrix
 
## The function makeCacheMatrix creates a special "vector". This vector is a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix
 
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        ## function to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## function to get the value of the matrix
        get <- function() x
        ## function to set the value of the inverse of a matrix
        setinverse <- function(solve) m <<- solve
        ## function to get the value of the inverse of a matrix
        getinverse <- function() m
        ## function to create a special "vector", which is a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 
}
 
## The following function calculates the inverse of the special "vector" created with the above function.
##  However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function.
 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                # If the value of Inverse is NOT null, then returns the cached data.
                return(m)
        }
        # If the value of Inverse is null, then calculates the inverse afresh.
        data <- x$get()
        m <- solve(data, ...)
        # Caching the calculated inverse
        x$setinverse(m)
        m
}
