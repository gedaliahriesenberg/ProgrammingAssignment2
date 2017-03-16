## Put comments here that give an overall description of what your
## functions do

## Returns a list containing functions to
##  1. set matrix
##  2. get matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) {

  Inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv= function(inverse) inv <<- inverse
  getinv = function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## output of makeCacheMatrix()
  ## return the inverse of the original matrix input to makeCaheMatr
  
  inv = x$getinv()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## otherwise..
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
  
}

