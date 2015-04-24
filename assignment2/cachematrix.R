## Creates a 'matrix' object that can cache its inverse
##This is based on the example
##I have changed vector to matrix, m to s, and mean to solve or solved

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) s <<- solve
  getsolved <- function() s
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}

## compute the inverse of the matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  ## If the inverse has already been calculated, inverse is retrieved from cache
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## Otherwise it's calculated here
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
