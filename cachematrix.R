## These two function are used to manipulate a matrix, to calculate its inverse and to store
## in cache. If inverse is found in cache it returns directly.

## This funtion returns a list of 4 functions to set and retrieve both matrix and its 
## inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function checks whether inverse is found in cache. If found it returns that
## directly otherwise calculates it and store in cache using other function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

