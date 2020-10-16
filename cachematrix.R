
## creates a special "matrix" object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" in case it was not calculated before


cachesolve <- function(x, ...) {
  inversed <- x$getinverse()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinverse(inversed)
  inversed
}



