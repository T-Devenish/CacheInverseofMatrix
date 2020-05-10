## This pair of functions will solve a matrix to work out the inverse
## and store it in a separate environment, where it can access the 
## inverse if it has already been worked out

## Function to create a matrix -- where we can save it's inverse in an
## environment that is separate from th current one 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to work out the inverse of a matrix returned from the function
## above, however it can get the answer from the cache if it has
## already been worked out

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}