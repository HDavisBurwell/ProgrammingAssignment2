##  The "makeCacheMatrix" creates a special "matrix" object that should cache its inverse. 
##  The "makeCacheMatrix" will contain 4 functions: set, get, setmean, getmean.
##  "get" is the function that returns the vector x stored. The "set" is a function 
##  that changes the vector. The "setmean and getmean" are similar to set and get but instead
##  they don't calculate the mean. It stores the value of the input in the variable z.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) z <<- solve
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The "cacheSolve" computes the inverse of them "makeCacheMatrix".
## The data gets the matrix stored with makeCacheMatrix, z calculates the inverse,
## and x$setmean(z) stores it in the object z in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
