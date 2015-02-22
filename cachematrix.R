## Given a matrix x, this function constructs 
## an 'extended' matrix object with a field holding 
## the inverse of x.
## Returns a list of four funtions:
## 1. set(x) : Sets the matrix
## 2. get()  : Returns the matrix
## 3. setInverse(y) : Sets the inverse of the matrix
## 4. getInverse()  : Resturns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(y) inverse <<- y
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Given a matrix object x constructed with the 'makeCachedMatrix' function,
## this function returns the inverse of x (if x is invertible).
## The inverse is computed only once and is later cached in the 
## 'inverse' field of x.
## If x is not invertible, this function will throw an error.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(data)
    inverse
}
