# Put comments here that give an overall description of what your
## functions do

## This function creates the cache for the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setInv <- function(solve) n <<- solve
    getInv <- function() n
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function shows the inverse of the "matrix" created in the last function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    n <- x$getInv()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setInv(n)
    n
}

