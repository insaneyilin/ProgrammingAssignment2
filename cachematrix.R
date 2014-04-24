## This file contains 2 functions for Caching the Inverse of a Matrix

## The first function, CacheMatrix, creates a special "Matrix", which is really 
## a list containing 4 functions: set, get, setInverse, getInverse.

## The second function, cacheSolve, calculates the inverse of the special 
## "Matrix" created with the above function. It first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setInverse 
## function.


makeCacheMatrix <- function(x = matrix()) {
        ## Take an argument x of type matrix
        ## Return 4 functions wrapped in a list
        inv <- NULL                  # set the cache's initial value to be NULL
        set <- function(y) {         # set x's value
                x <<- y
                inv <<- NULL
        }
        get <- function() x          # get x's value
        setInverse <- function(val) inv <<- val        # set cache's value
        getInverse <- function() inv       # get cache's value
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)      # return a list containing 4 functions
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()        # query the cache inverse
        if(!is.null(inv)) {          # if there is a cache
                message("getting cached data")
                return(inv)          # return the cache, no need to compute
        }
        data <- x$get()              # if there is no cache
        inv <- solve(data, ...)      # compute the inverse
        x$setInverse(inv)            # save the result in x's cache
        inv                          # return the result
}
