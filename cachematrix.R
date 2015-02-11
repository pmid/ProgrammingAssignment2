## makeCacheMatrix returns a list whose elements are functions
## it contains both the original matrix, m, and its inverse, if calculated, in s
## s contains the solve value, if set
## set() is not used
## get() returns the matrix stored when makeCacheMatrix is called
## setsolve stores the value of the inverse matrix in s
## getsolve returns s, which can be null or the calculated value, if available

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


## cacheSolve takes a list created by makeCacheMatrix as input
## if the inverse matrix has not yet been calculated, x$getsolve returns null;
##   in this case the value is calculated calling solve(), and cached calling setsolve()
## if the inverse has been calculated and set in the list created with makeCacheMatrix,
## it will be available, and solve() won't be called anymore

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
