
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the inverse matrix has not yet been calculated, this function 
## calculates it and returns it, but, if the inverse matrix was 
## previously calculated, this function gets the value of 
## the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data for previously inverse matrix calculated")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    message("inverse matrix uncalculated")
    inverse
}
