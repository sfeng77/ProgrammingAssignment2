
## These functions implements a matrix datatype that caches the inverse of the 
## matrix.


## This function constructs a matrix that supports the cached operation of 
## matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
    ## Returns a matrix that supports cached inverse operation.
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInverse <- function() inv
    setInverse <- function(inverse) inv <<- inverse
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## This function takes a cachematrix and computes its inverse.
## If an inverse is already available it skips the computation and return the
## inverse directly.
## Else, it computes the inverse and store it for future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', 
    ##where 'x' is the datatype constructed with the previous function.
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
