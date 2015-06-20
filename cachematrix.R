## Certain calculations are expensive and time consuming such as matrix inversion
## This cost can be reduced by caching the matrix in memory

## This function does four things
## 1. Set Matrix
## 2. Get Matrix
## 3. Set inverse of matrix
## 4. Get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function calculates the inverse of the matrix created from the above
## function. If the inverse is already in memory, returns the cached value
## else performs the computation and then returns the value
## This function assumes a square-invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
