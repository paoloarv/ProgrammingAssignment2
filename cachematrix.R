## Contains two related functions whose target is
## caching matrix inverse avoiding 
## time-consuming calculation of matrix inversion if it was
## already calculated in previous steps.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
    inverseMatrix <- NULL
    
    ## matrix changes, cache reset
    set <- function(y) {
        inputMatrix <<- y
        inverseMatrix <<- NULL
    }
    get <- function() inputMatrix
    setSolve <- function(solved) inverseMatrix <<- solved
    getSolve <- function() inverseMatrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, 
## then cacheSolve retrieves the inverse from the cache.
## Note: it assumes matrix supplied is always invertible

cacheSolve <- function(cacheMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- cacheMatrix$getSolve()
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    data <- cacheMatrix$get()
    s <- solve(data, ...)
    cacheMatrix$setSolve(s)
    s
}
