##  The 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
##  We use `solve` to compute the inverse of the `X` square matrix
##  We asume the given `x` matrix is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- Solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



##  The 'cacheSolve' either computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above or returns
##  the inverse from the cache if it has already been calculated

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
