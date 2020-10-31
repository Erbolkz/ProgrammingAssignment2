## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## THE FOLLOWING FUNCTION CREATES A SPECIAL "MATRIX" OBJECT THAT CAN CACHE ITS INVERSE
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## THIS FUNCTION COMPUTES THE INVERSE OF THE SPECIAL "MATRIX" RETURNED BY MAKECACHEMATRIX ABOVE. IF THE INVERSE HAS ALREADY BEEN CALCULATED (AND THE MATRIX HAS NOT CHANGED), THEN CACHESOLVE SHOULD RETRIEVE THE INVERSE FROM THE CACHE.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
