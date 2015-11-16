## Matrix inversion is usually a costly computation.
## The two functions below are beneficial because
## they cache the inverse of a matrix.

## Create a special "matrix" object that can cache 
## its inverse.  First set the value of the matrix,
## then get the value of the matrix, then set the
## value of the inverse of the matrix, and finally
## get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Compute the inverse of the object returned by
## makeCacheMatrix.  If the inverse has already
## been calculated and the matrix hasn't changed,
## then retrieve the inverse from the cache. If
## the inverse hasn't been calculated yet, 
## calculate it and set its value in the cache.

cacheSolve <- function(x, ...) {
                i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
