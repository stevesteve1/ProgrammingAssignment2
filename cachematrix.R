
## makeCacheMatrix creates a list of functions which have access to a 
## common environment created by the closed function makeCacheMatrix or 'cache'.  
## This enables the functions to get and set values of varaibles: x and inverse, which are 
## only accessible via these functions and not in the global envirnoment.  

## cacheSolve checks for a 'cached' inverse of the matrix data x.
## If found it returns that 'cached' inverse, if not it calcuates the inverse,
## stores it in 'cache' and returns it.


makeCacheMatrix <- function(x = matrix()) {
  ## inverse is initialised to NULL indicating that the inverse has not yet been calculated
  inverse <- NULL
  
  ## Create set function which assigns new data to x and sets inverse to NULL.
  ## The <<- operator is necessary to assign x and inverse in the environment of makeCachemMatrix
  ## rather than the environment of set. This allows x and inverse to be accessible to functions:
  ## get(), and getinverse()
  set <- function(y) {
    x <<- y
    inverse <<- NULL  ## inverse is re-initialized to NULL because a new matrix has been set
  }
  
  ## Create get function which returns the value of x from the makeCacheMatrix environment 
  ## i.e. the 'cached' value of x
  get <- function() x
  
  ## Create set inverse function which assigns a new value to invesre in the makeCacheMatrix environment
  setinverse <- function(inv) inverse <<- inv
  
  ## Create getinverse function which returns the value of inverse from the makeCacheMatrix environment
  getinverse <- function() inverse
  
  ## Output functions in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks for a cached inverse of the matrix data.
## If found it returns that cached inverse, if not it calcuates the inverse,
## stores it in cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Look for non-null inverse in 'cache' using getinverse()
  inverse <- x$getinverse()
  ## If non-null simply return cached inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Else retrieve the matrix data from 'cache' and compute inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  ## Update calculated inverse in 'cache' using setinverse() and return inverse
  x$setinverse(inverse)
  inverse
  
}
