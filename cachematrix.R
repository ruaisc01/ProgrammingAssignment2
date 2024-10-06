## These functions first create a special matrix that can
## cache its inverse, followed by a second function that
## computes the inverse of a special matrix. If the inverse
## matrix has already been calculated the second function
## should retrieve it.

## This function creates a special matrix that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special matrix.
## If the inverse matrix has already been calculated in the previous
## function it should retrieve it. 

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
        ## Return a matrix that is the inverse of 'x'
