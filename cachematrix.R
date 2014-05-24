###############################################################################
#
# This script contains functions that allow you to create a special type of
# matrix whose inverse can be cached. 
# 
# makeCacheMatrix() - given a matrix, this function returns a list of functions
#   for storing and retrieving the matrix and its cached inverse.
# cacheSolve() - compute the inverse of the matrix and cache the results.
#
###############################################################################

#
# makeCacheMatrix - Create a special type of matrix whose inverse can be cached
# 
# Args
#    x: The matrix
#
# Returns:
#    Functions for storing and retrieving the matrix and its inverse
#      set - store the matrix
#      get - return the matrix
#      setInverse - store the inverse of the matrix
#      getInverse - return the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  # Set the matrix and clear the inverse
  set <- function(mtx) {
    x <<- mtx
    i <<- NULL
  }
  
  # Return the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setinverse <- function(inv) i <<- inv 

  # Return the inverse of the matrix
  getinverse <- function() i
  
  # Return a list of subfunctions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#
# cacheSolve - Compute the inverse of a matrix and cache the result in order
#   to avoid calculating it again on subsequent calls.
# 
# Args
#    x: The special matrix created by calling makeCacheMatrix()
#
# Returns:
#    The inverse of the matrix 'x'
#
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
  } else {
    i <- solve(x$get())
    x$setinverse(i)    
  }
  i
}
