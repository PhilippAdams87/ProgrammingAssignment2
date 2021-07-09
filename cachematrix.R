## Caching the inverse of a matrix

## The first function creates a special matrix object to  its inverse
## loading the MASS package to make use of its inverse function for matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function () {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Cache function 

cacheSolve <- function(x, ...) {
  inv <-  x$getInverse()
  if(!is.null(inv)) {
    message("Retrieving cached data!")
    return(inv)
  }
  mat <- x$get
  inv <- solve(mat, ...)
  x$setInverse(inv)
}
