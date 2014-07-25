## Example use:
##> a<-diag(5000)*2 
##> b<-makeCacheMatrix(a)
##> cacheSolve(b) //calculates
##> cacheSolve(b) //gets from cache


## Adds caching ability to the given matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates inverse utilizing cache when possible
cacheSolve <- function(x, ...) {
  ## Checks if it is in cache
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## If not in cache calculates and saves it to cache
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
