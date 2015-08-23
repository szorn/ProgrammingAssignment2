## Functions used to cache the value of matrix inversion, to eliminate repeating potentially
## time-consuming computations if the value has already been calculated.
## Two functions:  makeCacheMatrix() -- creates the list of functions to get/set matrix and inverse
##                 cacheSolve() -- calculates or retrieves inverse of matrix       

## Create a list that contains functions to:
##    set: set the value of the list (to a matrix)
##    get: get the value of the list (a matrix)
##    setInverse: set the value of the inverse of the matrix
##    getInverse: get the value of the inverse of the matrix     

makeCacheMatrix <- function (x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(solve) m <<- solve
    
    getInverse <- function() m
    
    list (set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'.
## If the inverse has already been calculated, print a message and
## return the cached value. Otherwise, calculate the inverse and save
## the value.

cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    
    ## if it already exists, return the cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  
    }
    
    ## else, calculate the inverse, cache the value, and then return the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
