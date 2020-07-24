## These two functions work together, either to calculate the inverse matrix of a given input matrix, or retrieve it from the cache##

## This function will initialize the values of x and m, as well as generate the four functions get, set, getinverse, setinverse##

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function (y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


##This function will either retrieve the cached inverse matrix, or use the solve function to generate the inverse matrix##

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)){
      message ("getting cached data")
      return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

##example of invertible matrix##
A = matrix(
  c(2,3,2,2),
  nrow = 2,
  ncol = 2,
  byrow = TRUE)
