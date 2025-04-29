## These functions create a special object that stores a matrix 
## and caches its inverse.This is useful when matrix inversion
## is computationally expensive and needs to be reused.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # Set a new matrix
    m <<- NULL # Reset the cached inverse
  }
  get <- function() x # Return the current matrix
  setinverse <- function(inverse) m <<- inverse # Cache the inverse
  getinverse <- function() m # Retrieve the cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then it retrieves 
## the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")  # Inform the user that cached data is used
    return(m)                       # Return the cached inverse
  }
  data <- x$get()        # Get the matrix
  m <- solve(data, ...)  # Compute the inverse
  x$setinverse(m)        # Cache the inverse
  m                      # Return the newly computed inverse
}
