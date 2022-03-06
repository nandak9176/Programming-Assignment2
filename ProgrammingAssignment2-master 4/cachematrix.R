## Find the results of makeCacheMatrix in order to solve for cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cache and find the inverse of said matrix
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(inv)
  m
}