## Listed below are functions that provide access to 
## a matrix object with cached inverse calculation 

## This function creates an extended matrix object 
## and provides it with "set" and "get" methods to set 
## and get the value of the matrix and "setinv" and
## "getinv" methods to set and retrieve the values of 
## the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrInv <- NULL
  set <- function(y) {
    x <<- y
    matrInv <<- NULL
  }
  get <- function() x
  setinv <- function(invval) matrInv <<- invval
  getinv <- function() matrInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function returns cached version of the inverse
## corresponding to the argument extended matrix object
## or if cached inverse if unavailable calls "solve" 
## to calculate the inverse, then caches and returns 
## the result. NULL is returned for cases when inverse
## is not defined for a given matrix.

cacheSolve <- function(x, ...) {
  ## Check if X has its inverse cached
  matrInv <- x$getinv()
  if(!is.null(matrInv)) {
    message("getting cached data")
    return(matrInv)
  }
  
  ## If X doesn't have its inverse cached, 
  ## prepare to calculate the inverse. 
  ## Retrieve matrix value.
  data <- x$get()

  ## Calculate the inverse. 
  matrInv = tryCatch({
    solve(data, ...)
  }, warning = function(w) {
    NULL
  }, error = function(e) {
    NULL
  })
 
  ## Cache the inverse
  x$setinv(matrInv)
  
  ## Return the inverse
  matrInv  
}
