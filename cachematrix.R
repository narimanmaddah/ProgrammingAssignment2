## Matrix inversion is usually a costly computation
## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

# This function creates a cache matrix and provides methods to set, get,
# set inverse and get inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse as NULL
  inv <- NULL
  # set method sets the matrix and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get method returns the matrix
  get <- function() x
  # setinv method sets the inverse of the matrix
  setinv <- function(i) inv <<- i
  # getinv method returns the inverse of the matrix
  getinv <- function() inv
  # return a list of all the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Function to solve the inverse of a matrix and cache the result

cacheSolve <- function(x) {
  # Get the cached inverse of the matrix
  inv <- x$getinv() 
  if (!is.null(inv)) {
    # Display a message indicating that cached data is being used
    message("Getting cached data")
    # Return the cached inverse if available
    return(inv)
}
  # Get the matrix data
  data <- x$get()
  # Calculate the inverse of the matrix
  inv <- solve(data)
  # Cache the inverse of the matrix
  x$setinv(inv)
  # Return the inverse
  inv
}
