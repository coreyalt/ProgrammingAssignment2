## This script contains functions to build an object
#containing a matrix and a cached inverse of that matrix


## makeCacheMatrix creates a "matrix" with a cached inverse.
# This matrix is in fact a list containing four functions: 
# set, get, setinv, and getinv. These functions respectively 
# return or set the attributes of this "matrix," the matrix
# itself, or the cache. The function does not compute and cache
# the inverse of the matrix by default

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes "matrix" as an argument, checks whether said
# matrix has a cached inverse already, and either returns the 
# inverse stored in the cache or computes, stores, and returns 
# the inverse of the matrix passed

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    x$getinv()
  }
  else{
    inv <- solve(x$get())
    x$setinv(inv)
    x$getinv()
  }
  
}

