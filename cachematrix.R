## This file contains my solution to Assignment 2.
##There are two functions: makeCacheMatrix and cacheSolve, 
##which allow for quicker calculations of an inverse.
##The results are cached, which prevents having to calculate
##the same inverse twice

##makeCahceMatrix(matrix) provides the getters and setters to
##determine if the calculation has been done before and can be
##found in the cache. Call this function first to get the matrix
##to pass into cacheSolve(matrix) below

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set , get = get, setinv = setinv, getinv = getinv)  
}


##cacheSolve(matrix) takes a matrix from makeCacheMatrix(matrix), above
##and checks to see if the value has already been
##calculated. It only calculates the inverse if it has not already
##been stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
      message("getting cached data...")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
