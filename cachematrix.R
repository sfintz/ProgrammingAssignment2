# Coursera - JHU - R Programming - Programming Assignment 2 
# Caching the Inverse of a Matrix

## overview:  makeCacheMatrix creates a special "matrix" and 
## cacheSolve computes the matrix inverse of special matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the matrix inverse (MI)
##    get the value of the matrix inverse (MI)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMI <- function(solve) m <<- solve
  getMI <- function() m
  list(set = set, get = get, setMI = setMI, getMI = getMI)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  m <- x$getMI()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMI(m)
  m
}
