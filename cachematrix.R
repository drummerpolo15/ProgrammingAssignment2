## Matrix inversion is usually a costly computation and their may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly. The pair of functions below cache the inverse of a matrix.



## This function creates a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversesolved) inverse <<- inversesolved
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## This function only computes the inverse of a matrix that can be inverted

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...)
  x$setinverse(inverse)
  inverse
}
