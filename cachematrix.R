##  The following functions cache the inverse of a given matrix
##  so that it can be quickly recalled later, rather than 
##  computing it again.
##
##  makeCacheMatrix equips a matrix with the ability to remember
##  its inverse as computed by cacheSolve. The inverse can then be
##  recalled quickly by cacheSolve.
##
##----------------

##  makeCacheMatrix takes a matrix and returns a list of functions:
##      set - inputs a new matrix
##      get - accesses the input matrix
##      setinv - stores the inverse of the input matrix
##      getinv - accesses the inverse, once stored

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
                setinv = setinv, 
                getinv = getinv)
}


##  cacheSolve finds the inverse of the matrix which was passed
##  to makeCacheMatrix. If the inverse has already been stored, 
##  then the function retrieves the stored inverse. Otherwise the
##  function computes, stores, and returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
