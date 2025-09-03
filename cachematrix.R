## Returns a list of functions that support caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # The cached inverse
  
  set <- function(y) {
    x <<- y # set the matrix
    inv <<- NULL # invalidate the cache
  }
  
  get <- function() x
  
  setinv <- function(i) inv<<-i
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x,...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  m<-x$get()
  inv <- solve(m,...)
  x$setinv(inv)
  inv
}
