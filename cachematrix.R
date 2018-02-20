# Functions to calculate inverse of a matrix or return 
# a cached value if the same matrix inversion have already
# been calculated.

# Usage example:
# m <- matrix(rnorm(100), 10, 10)
# x <- makeCacheMatrix(m)
# cacheSolve(x)
####################################################
# Create a cachable "matrix"

makeCacheMatrix <- function(x = matrix()) {
    # inverse of matrix initialisation
    i <- NULL
    
    # setter function for original matrix
    set <- function(y){
        x <<- y
        i <<- NULL 
    }
    
    #getter function for original matrix
    get <- function() x
    
    # setter for inverse of matrix
    setinverse <- function(solve) i <<- solve
    
    #getter for inverse
    getinverse <- function() i
    
    # available ops
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Check whether inverse calculation have already
# been done. Returns cached value if available, 
# otherwise calculate the inverse and cache it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
