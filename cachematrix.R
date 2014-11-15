## Cached Matrix creation and inversion (solve) functions
## Lazily calculates the inverser of the matrix and caches the result

## Create a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Inv a matrix once, and caches the result.  Further calls on the same
## matrix will return the pre-computed value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' or solve a set of linear
    ## equations

    # If other parameters have been passed then the caller may be solving
    # linear equations, so don't use the cache since the result will
    # depend on the other arguments
    args  <- list(...)
    if (length(args) != 0){
        return(solve(x$get(), ...))
    }

    inv <- x$getinv()
    if(!is.null(inv)) {
        # message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
