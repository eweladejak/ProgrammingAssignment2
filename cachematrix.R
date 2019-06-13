## Pair of functions that cache the inverse of a given matrix


## 1. Function thah creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <<- function(y){
   x <<- y
   inv <<- NULL
 }
  get <- function() x
  setinv <- function(ver) inv <<- ver
  getinv <- function() inv
  # create a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 2. Function thah computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # check if the inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    # return the inverse from the cache
    return(inv)
  }
  # compute the inverse (if it hasen't been calculated yet)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
