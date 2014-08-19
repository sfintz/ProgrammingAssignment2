## overview:  makeCacheMatrix creates a special "matrix" and cacheSolve computes the matrix inverse of
##  special matrix

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


## The following function calculates the matrix inverse (MI) of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the matrix inverse has already been calculated. If so, 
## it gets the matrix inverse from the cache and skips the computation. Otherwise, it 
## calculates the matrix inverse of the data and sets the value of the matrix inverse in 
## the cache via the setMI function.

cacheSolve <- function(x, ...) {
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
