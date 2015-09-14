## Pair of functions to create and use a special data type that
## caches matrix inversions. Uses R environments and lexcical
## scoping rules to implements the cache.


## Create a matrix that can cache it's inverse 
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
    xInverse <<- inverse
  }
  getInverse <- function() {
    xInverse
  }
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Invert the matrix (directly return a cached version if available)
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}

