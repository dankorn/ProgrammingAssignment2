## Programming Assigment 2

## This function creates a special "matrix" object that can cache its inverse.
## $set() establish a new value
## $get() gives the value
## $setinv() establish a new cache inverse value
## $getinv() gives the inverse value
## the input x must be a matrix

makeCacheMatrix <- function(x = matrix()) {
  if (class(x)!="matrix") {
    print('x must be a matrix')
    stop()
  }
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xInv <<- inv
  getinv <- function() xInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  xInv <- x$getinv()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setinv(xInv)
  xInv
}
