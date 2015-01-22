## Manages matrices with a cached inversion result

## Factory to build matrices with caching mechanism included for inversion
## inputs x : initial value
##  returns : a list with 4 items, acting as a matrix with caching mechanisme
##           for inversion

makeCacheMatrix <- function(x = matrix()) {
  
  ## sets the inverse to NULL to mark it isn't calculated yet
  ## stores it in parent environment (global env)
  inverse <<- NULL
  
  ## changes stored matrix value
  set <- function(y) {
    x <<- y
    ## since the value has changed, any cached inverse is no longer valid
    inverse <<- NULL 
  }
  
  get <- function() x
  
  ## for internal use by cacheSolve
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  ## packages all four functions into a list
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given a matrix build with makeCacheMatrix, returns the inverse
## from the cache or calculate and cache it if it's not already there
## inputs : x, matrix build with makeCacheMatrix
##  returns : a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    # the inverse was found in the cache, we just return it and stop here
    message("Fetching inverse from cache")
    return (i)
  }
  # No inverse in cache : we need to calculate it
  matrix <- x$get()
  # For this assignment, assume that the matrix supplied is always invertible.
  # No need to check for errors
  inverse <- solve(matrix)
  # Once calculated, the inverse is stored in the cache
  x$setInverse(inverse)
  inverse
}
