## Functions for caching the Inverse of a Matrix 
## Cousera - R Programming - Week3 

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Set/Get for source matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## Set/Get for inversed matrix
  setinv <- function(invert) inv <<- invert
  getinv <- function() inv
  
  ## Return list with ready methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        
  ## Try to use the cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the cache is empty, we solve
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## And print result
  inv
  
}
