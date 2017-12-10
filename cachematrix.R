## The following functions cache a matrix 'x' that is invertible.  

## makeCacheMatrix returns a list of functions to set the matrix, get the matrix, 
## set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(x) inv <<- solve(x)
        getinv <- function() inv
        x<<-list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve uses the list of functions from makeCacheMatrix and returns the inverse
## of a matrix.

cacheSolve <- function(y, ...) {
        inv <- x$getinv()
        ## if the inverse has already been calculated, it returns the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(x$getinv())
        }
        ## otherwise, the function calculates the inverse
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
