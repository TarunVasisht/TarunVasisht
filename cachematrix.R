## The aim is to write a function to create a special matrix that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversemat <<- inverse
  getInverse <- function() inversemat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function returns the inverse of a matrix or obtain the inverse from the cache if the inverse is already found
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inversemat <- x$getInverse()
  if (!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  mat <- x$get()
  inversemat <- solve(mat, ...)
  x$setInverse(inversemat)
  inversemat
}

