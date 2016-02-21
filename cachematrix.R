##=======begin makeCacheMatrix function==========

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##======end makeCacheMatrix function============


##======begin cacheSolve function===============
## The cacheSolve function computes the inverse of the special "matrix" 
## which is returned by the makeCacheMatrix function (found obove). 
## If the inverse has already been calculated
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  i <- x$geti()
   if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  ## solve returns the inverse of a square invertible matix
  x$setinv(i)
  i
}
##=====end cacheSolve function===================

