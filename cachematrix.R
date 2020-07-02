## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## Set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x ## Get the value of the matrix
    setinverse <- function(solve) m <<- solve() ## Set the inverse of the matrix
    getinverse <- function() m ## Get the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return the inverse matrix of x
    m <- x$getinverse() ##get the inverse as set in the makeCacheMatrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m) ##return the cache computed inverse matrix
    }
    matrx <- x$get()
    m <- solve(matrx, ...)
    x$setinverse(m)
    m
}
