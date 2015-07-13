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
## output   : list containing the constructed functions set,get,setinv and getinv
##---------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    #--- initialize the inverse matrix
    inv <- NULL
    
    #--- define the setter/getter functions
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    #--- return the output list object defining all functions
    return(list(set = set , get = get, setinv = setinv, getinv = getinv))
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
    #--- retrieves the inverse matrix
    inv <- x$getinv()
    
    #--- check if it is cached and return it if not null
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #--- otherwise calculate a new inverse matrix and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    #--- return the newly calculated inverse matrix
    return(inv)
}

##---------------------------------------------------------------------------------------
## Testing - uncomment the following lines to test the two functions declared above
##---------------------------------------------------------------------------------------
#--- Should declare a list of functions for a 3x3 random input matrix 
#m <- makeCacheMatrix( matrix(sample(1:9),nrow=3))

#--- Should display the inverse matrix of a 3x3 matrix
#cacheSolve(m)

#--- Should display the 'getting cached data' message and the inverse matrix
#cacheSolve(m)

