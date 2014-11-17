## Calculate the inverse of a square matrix and cache the result as part of a special matrix object (really a list)
## Created by callmejho

## makeCacheMatrix(x = matrix())
## Return a special matrix object based on x that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x, ...)
## Return the inverse matrix of x. 
## If the inverse has not been cached, calculate and cache. 
## If the inverse has been cached, return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
