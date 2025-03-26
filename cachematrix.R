## These functions work together to cache the inverse of a matrix
## This helps avoiding re-computing the inverse multiple times.

## Creates a special "matrix" object that can cache its inverse.
## It contains functions to set and get the matrix, as well as set and get its inverse..

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y 
                inv <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix stored in the special object created by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the cached value instead of recomputing it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## Return a matrix that is the inverse of 'x'

        if (!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

