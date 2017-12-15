## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object is created that exposes methods allowing 
## original and inverse matrix to be get/set

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes the special matrix object created by makeCacheMatrix and tests if 
## the inverse of the original matrix is available 
## If not available, generate inverse and cahce.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # check if we have a cached matrix already
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Solve, cache and return result
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}