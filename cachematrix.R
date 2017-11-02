## Put comments here that give an overall description of what your
## functions do

## This function 

makeCacheMatrix <- function(x = matrix()) {
  ##set inverse to start null
  inv <- NULL
  
  #set y = matrix(x) and inv to null 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  ##set inverse to inv
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  ## if inv is not null, will pull back cached inv
  if (!is.null(inv)) {
    message("pulling cached data")
    ##returns cached matrix
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}