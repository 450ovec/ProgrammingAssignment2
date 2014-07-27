## Create a special "matrix", which is in fact a list containing functions to:
## set the matrix, get the matrix, set the inverse of the matrix, get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NA
    
    set <- function(y) {
        x <<- y
        inv <<- NA
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate the inverse of the special "matrix" created with the makeCacheMatrix.
## If the inverse has already been calculated it'll be taken from the cache with no computation.
## Otherwise, the inverse of the matrix will be calculated and stored in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.na(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
