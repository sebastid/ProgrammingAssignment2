#########################################################################################
## This script contains two functions:
## 1 - makeCacheMatrix : to create a cacheable inverse matrix 
## 2 - cacheSolve      : to return a matrix inverse exploiting the cacheable 
##                       inverse matrix functions created by makeCacheMatrix.
#########################################################################################

##---------------------------------------------------------------------------------------
## function : makeCacheMatrix
## desc     : This function creates an extended matrix object that can cache its inverse.
## input    : x, matrix 
## output   : list containing the functions set,get,setinv and getinv
##---------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set , get = get, setinv = setinv, getinv = getinv)
}

##---------------------------------------------------------------------------------------
## function  : cacheSolve
## desc      : This function calculates the inverse of a matrix object returned by makeCacheMatrix.
##             If the inverse exists already and the matrix has not changed then the cacheSolve function retrieves 
##             the inverse matrix from the cache.
## input     : x, list object created by the makeCacheMatrix function
##             ..., other arguments that used by the solve command
## output    : the inverse of the input 
##---------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##---------------------------------------------------------------------------------------
## Testing
##---------------------------------------------------------------------------------------
m <- makeCacheMatrix( matrix(sample(1:9),nrow=3))
# Should display the inverse matrix
cacheSolve(m)
# Should display the 'getting cached data' message and the inverse matrix
cacheSolve(m)

