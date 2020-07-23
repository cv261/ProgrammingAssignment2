## The following are the functions that cache and compute the 
## inverse of a matrix

## the function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}


## This function computes the inverse of the matrix
## returned by "makeCacheMatrix" above. If the inverse
## has already been calculated (the same matrix), then
## this function can retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
