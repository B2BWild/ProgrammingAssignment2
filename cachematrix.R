## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix - contains special function atributes in order to cache the inverse of a matrix
#                   Returns functions: set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) INV <<- solve
  getinverse <- function() INV
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve calculates the cached inverse of a matrix contained in a makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV <- x$getinverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setinverse(INV)
  INV
}
