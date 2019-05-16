## The functions makeCacheMatrix and cacheSolve works
## together to solve the matrix inverse calculation
## problem using cached values. makeCacheMatrix is
## used to create the cache to store the inverse. The
## method cacheSolve first checks whether the result
## is already calculated and stored in the cache. If
## it is, the method simple gets the inverse from the
## cache in makeCacheMatrix(). Otherwise, the inverse
## is calculated and returned. In addition, the result
## is stored in the cache

## This method implements the cache to store the result
## of matrix inverse calculation. It always expects a
## square and invertible matrix as input
makeCacheMatrix <- function(x=matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3)) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse_x) x_inverse <<- inverse_x
  getinverse <- function() x_inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## This method first check whether the cache contains the
## the result of the inverse calculation. If it does, then
## the resulting inverse if retrieved from the cache.
## Otherwise the inverse is calculated, stored in the cache
## and returned to the user

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  
  if (!is.null(x_inverse))
  {
    message("Getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data)
  x$setinverse(x_inverse)
  x_inverse
}
