## These functions are used to cache the inverse of a matrix
## makeCache(m) creates a special cached list of functions from a matrix m.
## cacheSolve(m) returns the inverse matrix m (created in makeCache)
## either:
##    1. from cache
##    2. Calculates the inverse and caches it.

## makeCacheMatirx makes a special list of functions
##  1. set the value of the matrix
##  2. get the value of matrix
##  3. set the value of the inverse
##  4. get the value of the inverse
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolves will either
##  1. Return the cached inverse of the special matrix m.
##  2. Calculate the inverse of special matrix m and cache the inverse.

cacheSolve <- function(m, ...) {
    i<- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- m$get()
    i <- solve(data)
    m$setinverse(i)
    i
}