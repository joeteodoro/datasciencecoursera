## Caching the Inverse of a Matrix
## This function assumes that the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- mean
  getmatrix <- function() m
  matrix(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the matrix inverse has already been calculated and matrix has not changed then this function 
## should retrieve the matrix inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- matrix(data, ...)
  x$setmatrix(m)
  m

}
