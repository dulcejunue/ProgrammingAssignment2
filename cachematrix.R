## Put comments here that give an overall description of what your
## functions do

## This object contains two attributes x and xInv. The first is a matrix that
## is given, and xInv is to store the inverse of x. For both attributes we have
## their corresponding set and get functions. is important to assign NULL to 
## xInv in case x is changed, since the inverse wont be the same, and must be
## assigned again.

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(otherInverse) xInv <<- otherInverse
  getInverse <- function() xInv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This functions receives a makeCacheMatrix object, x, and returns the
## inverse matrix associated to it, in case it has not been computed, it
## extracts the matrix associated to x and calculate its inverse using 
# solve(), then assign the recently computed inverse matrix to x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  
  oriMatrix <- x$get()
  inv <- solve(oriMatrix)
  x$setInverse(inv)
  
  inv
}
