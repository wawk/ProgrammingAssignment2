## This function takes a parameter of a square matrix
## It then constructs an internal list of 4 functions (set,get,setsolve,getsolve)
## The object returned from the makecacheMatrix is a list of these functions
makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
  
  ## Function  takes as a parameter a matrix (inverted)
  ## and sets it to a global variable (x)
  ## This function is not being called by cacheSolve but can be explicitly called
  ## to reset the initial (non cached) matrix by using the return result from makeCacheMatrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Function to return the square matrix (non inverted) passed in to makeCacheMatrix
  get <- function() x
  
  ## store the inverted matrix passed in from cacheSolve in the cache
  setsolve <- function(m_inverted) m <<- m_inverted
  
  ## Get the cache store for the inverted matrix (may be Null if first time accessed)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function that takes a parameter of a makeCacheMatrix object
## and calls either get or set on the cache of the inverted matrix
## This function returns an inverted matrix, messaging when the cache
## inverted matrix is returned (no message for a new inverted matrix)
cacheSolve <- function(x, ...) {
  
  ## The cache for the inverted matrix is either populated (from previous run)
  ## or it is null (initial state)
  m <- x$getsolve()
  if(!is.null(m)) {
    
    ## Since there is already a cached value return a message indicating
    ## where the data is coming from.
    message("getting cached data")
    
  } else {
    ## There was no cached data get the original matrix and invert it
    data <- x$get()
    m <- solve(data, ...)
    
    ## Call the setsolve function to update the cache with the new inversion
    ## of the matrix
    x$setsolve(m)
  }
  
  ## And return the inverted matrix
  m
}
