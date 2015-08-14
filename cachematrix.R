## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a "matrix", by taking and caching an initial matrix, x and defining the
# following list of operations:
#
# set(y) -  replace the cached matrix with a new matrix, y, uncaching its inverse
# get() - return the current cached matrix
# setinverse(inverse) - set and cache the inverse of x
# getinverse() -  return the currently cached inverse of x, or NULL if this has yet
#                 to be computed
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { inv <<- inverse }
  getinverse <- function() { inv }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# Calculate the inverse of the matrix represented by the list, x, created using
# the makeCacheMatrix function. The inverse returned is the value cached by
# makeCacheMatrix if this has already been computed, otherwise the solve function
# is called and the result cached into x.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
