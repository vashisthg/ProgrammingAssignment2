# Example usage:
# > matrix <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cacheMatrix <- makeCacheMatrix(matrix)                  // Create our special matrix
# > cacheMatrix$get()                                  // Return the matrix
# > cacheSolve(cacheMatrix)                            // Return the inverse
# > cacheSolve(cacheMatrix)                            // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of t

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("cache hit")
        return(inverse)
    } else {
        message("cache miss")
    }
    
    data <- x$get()
    inverse <-solve(data)
    x$setInverse(inverse)
    inverse
}


