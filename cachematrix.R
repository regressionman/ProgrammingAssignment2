## The functions below, when used together, illustrate how an R function is able
## to cache potentially time-consuming computations.  It uses the <<- operator
## to assign a value to an object in an environment different from the current
## environment.  These two functions are used to create a special object that
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix())
    {
    
    ## The following function creates a special "matrix" object that can cache its inverse
    inv <- NULL
  
    setmatrix <- function(y) {

      ## The <<- operator will assign the argument, y, to x in the gloabl environment
      x <<- y

      ## When the matrix is set, inv must be "reset" to NULL      
      inv <<- NULL
      }
    
    getmatrix <- function() x
    
    setinverse <- function(inverse) inv<<-inverse
    
    getinverse <- function() inv
    
## Return the association between setmatrix, getmatrix, setinverse, and
## getinverse and their function.
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}
  

## The following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix.  If the inverse has already been calculated, it will be returned.
## Otherwise, cacheInverse will compute, set, and return the inverse.

cacheInverse <- function(x, ...) {

    inv <- x$getinverse()

## If the inv matrix isn't NULL, then it has already been computed so return its value
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }

##  Get the martix, compute its inverse, set its value and return the inv matrix
    data <- x$getmatrix()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}