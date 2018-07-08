##  these are functions that cache the inverse of a matrix.
## solve the inverse of the special Matrix and cache.

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x <<- y
    f <<- NULL
  }
  get <- function() x
  inv <- function() {
    f <<- solve(x)
  }
  getinv <- function() f
  list(set = set,get = get,inv = inv,getinv = getinv)
}

## if the inverse has aleady been calculated so return the cache.If not,
##caculate it.

cacheSolve <- function(x, ...) {
  f <- x$getinv
  if (!is.null(f)) {
    message("getting cached inverse of the Matrix")
    return(f)
  }
  data <- x$get()
  f <- solve(data)
  return(f)
}
