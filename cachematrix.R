## Created by Taras Smereka on Jan 25th 2016

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        setData <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        getData <- function() {
                x
        }
        setInverse <- function(inv) {
                cachedInverse <<- inv
        }
        getInverse <- function() {
                cachedInverse
        }
        list(setData = setData, getData = getData, setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## `cacheSolve` assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Computing the inverse of a square matrix can be done with the `solve`
        ## function in R. For example, if `X` is a square invertible matrix, then
        ## `solve(X)` returns its inverse.
        
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$getData()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
