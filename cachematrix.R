## Contains two related functions whose target is
## caching matrix inverse avoiding 
## time-consuming calculation of matrix inversion if it was
## already calculated in previous steps.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    ## matrix changes, cache reset
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
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

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
