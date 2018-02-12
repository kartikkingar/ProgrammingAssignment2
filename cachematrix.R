## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
j <- NULL
  set <- function(y) { 
    x <<- y
    j <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) j <<- inverse 
  getinverse <- function() j 
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       k <- x$getinverse()
  if (!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}
