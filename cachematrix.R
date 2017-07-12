## This is a cachable solution to matrix inversion. Matrix inversion, like solve
## function, take long to run. So, if we can cache inverted matrix from previous
## computation, if matrix didn't change, we can simple return previous calculated
## inversion
## first function is responsible for caching original matrix and inversion.
## second function wraps R solve funtion to first cache matrix

## This function receives a simple invertable matrix as argument and manage its
## inversion cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # Aux var with inverted matrix
    set <- function(y) { # sets a new matrix value and reset inverted matrix value
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse ## cache inverted matrix
    getInverse <- function() i ## return cached inverted matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function wraps R solve function to add cache capabilities

cacheSolve <- function(x, ...) {
        i <- x$getInverse() # first try to recover cached inversion
        if (!is.null(i)) {
            message("working with cached data")
            return(i)
        }
        data <- x$get() # Recover original matrix
        i <- solve(data, ...) # Try to calculate inverse of data with other arguments
        x$setInverse(i)
        i
}
