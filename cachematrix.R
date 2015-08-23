## The following functions were created to compute and cache the 
## inverse of a matrix.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by 
## the function: makeCacheMatrix()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

