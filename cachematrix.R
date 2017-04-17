## Developed by Rui Almeida as a requirement to:
## R programming: Week 3 Assignment
## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invertida <- NULL
        set <- function(y) {
                x <<- y
                invertida <<- NULL
        }
        get <- function() x
        setinvertida  <- function(solveMatrix) invertida <<- solveMatrix
        getinvertida  <- function() invertida
        list(set = set, get = get,
             setinvertida = setinvertida,
             getinvertida  = getinvertida )
}


## This function computes the inverse of the matrix created by the above function. 
##If the inverse has already been calculated (and the matrix has not changed), hen cacheSolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invertida <- x$getinvertida()
        if(!is.null(invertida)) {
                message("getting cached data")
                return(invertida)
        }
        data <- x$get()
        invertida <- solve(data)
        x$setinvertida(invertida)
        invertida
}
