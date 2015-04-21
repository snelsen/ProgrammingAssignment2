##
## cachematrix.R - This file contains functions which facilitate the
##  efficient computation of the inverse of a matrix through the utilization
##  of a simple cache of the inverse associated with each matrix.
##

## makeCacheMatrix - This function returns a special matrix type which contains
##  the following accessors:
##  * set - set the matrix
##  * get - get the matrix
##  * setInverse - set the inverse of the matrix
##  * getInverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of a special matrix of a type defined
##  by the makeCacheMatrix() function.  It first checks to see if the matrix
##  already has a cached inverse.  If so, it returns that; otherwise, it
##  computes the inverse and saves it in the special matrix before returning
##  the value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    inv <- solve(x$get())
    x$setInverse(inv)
    inv
}
