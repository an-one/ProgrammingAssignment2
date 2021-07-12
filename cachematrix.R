## The first function makeCacheMatrix creates a matrix object that can cache its inverse. 
## The second function cacheSolve will create the inverse of matrix  
## only if that inverse was not created previously

makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) matrixInv <<- inverse
    getinv <- function() matrixInv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}




cacheSolve <- function(x, ...) {
       
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
