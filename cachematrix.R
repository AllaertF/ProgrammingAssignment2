## Create a special type of matrix that can cache its own inverse
## and a function to compute this inverse or retrieve it from the 
## cache.

## Create a "special" matrix object that can cache its own inverse.
## This function, in fact, returns a list of 4 functions to get or 
## alter the actual matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute, set and return the inverse of a "special" matrix as
## constructed by the makeCacheMatrix() function. If the inverse 
## has already been calculated, simply get it from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
