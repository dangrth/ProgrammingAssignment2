## Manages matrices with a cached inversion result

## Factory to build matrices with caching mechanism included

makeCacheMatrix <- function(x = matrix()) {

  inverse <<- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given a matrix build with makeCacheMatrix, returns the inverse
## from the cache or calculate and cache it if it's not already there

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    # the inverse was found in the cache
    message("Fetching inverse from cache")
    return (i)
  }
  # No inverse in cache : we need to calculate it
  matrix <- x$get()
  # For this assignment, assume that the matrix supplied is always invertible.
  # No need to check for errors
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
