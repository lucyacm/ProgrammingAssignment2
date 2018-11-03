## The following functions are used to create a special object that 
## stores a matrix and caches its inverse.
## Note: It is assumed that the matrix supplied is always invertible.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. This object is really a list containing 
## a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y) {
                        x <<- y
                        s <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list( set = set, 
                  get = get,
                  setsolve = setsolve,
                  getsolve = getsolve) 
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above. 
## If the inverse of the matrix has already been calculated, then 
## the cacheSolve function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s) ) {
              message("getting cached data")
              return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
