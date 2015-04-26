## Create a functions that stores a list of functions
## List of functions in makecacheMatrix: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Set the value to null
  set <- function(y) { # Use this function to set argument
    x <<- y 
    m <<- NULL
  }
  get <- function() x  # Get the argument
  setinv <- function(inv) m <<- inv # Set the matrix inverse
  getinv <- function() m  # Get the updated matrix inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  # list of functions
}

## cacheSolve function check if the matrix inverse is calculated, if not calculates the matrix inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
