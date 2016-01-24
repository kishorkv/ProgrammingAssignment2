## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly, following functions will help create special matrix 
## and to cache the inverse of that matrix



## This function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache 
cacheSolve <- function(x, ...) {
  # Check you have cached data
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting already cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
