## makeCacheMatrix returns a list whose elements are functions
## it contains both the original matrix, x, and its inverse, if calculated, in s
## - s contains the cached value, if already set
## - set() is not actually used by cacheSolve; 
##     it can be used from outside to replace the matrix x with another one, in that case the cached value
##     will be cleaned at it will be obsolete
## - get() returns the original matrix (x) stored when makeCacheMatrix is called
## - setsolve caches the value of the solve value in s
## - getsolve returns the cached value, which can be null or valid
## - a list is returned, containing the four functions defined. Accessing the items 
##     (e.g. {cm  <- makeCacheMatrix(m); cm$get() } ) will actually call the specified function.
##     In this case, the get() function will be called, and the original matrix will be returned

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve takes an object (list) created by makeCacheMatrix as input
## - if the inverse matrix has not yet been calculated, x$getsolve returns null;
##     in this case the value is calculated calling solve(), and cached calling x's setsolve()
## - if the inverse has been calculated and set in the list created with makeCacheMatrix,
##     it will be available in next calls to x$getsolve(), and solve() won't be called anymore
## - either the cached or the calculated value (m) is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
