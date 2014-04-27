## Put comments here that give an overall description of what your
## functions do

### Functions for setting and getting the value of a matrix and
### for caching the inverse of the argument, a matrix x. 
### The argument matrix is assumed to be invertible.


## Write a short comment describing this function

### Function makeCacheMatrix contains code for  
### initializing an empty matrix and then setting and caching a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(numeric(0), 0,0)    
  set <- function(y) {            
    x <<- y
    m <<- matrix(numeric(0), 0,0) 
  }

  ### Returns a list of functions that can be called individually
  get <- function() x             
  setmatrix <- function(matrix) m <<- matrix()
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      
  ## If matrix isn't empty, returns the cashed matrix m
  m <- x$getmatrix()           
  if (length(m) != 0) {
    message("getting cached data")
    return(m)
  }
  
  ## If the matrix is empty, then the inverse of the argument matrix
  ## is computed and returned
  data <-x$get()               
  m <- solve(data, ...)       
  x$setmatrix(m)
  m
  
}