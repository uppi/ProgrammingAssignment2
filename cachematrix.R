## A pair of functuions that cache the inverse of a matrix.


## Create an instance of matrix
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  ## Set new matrix and clear the inverse cache
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  ## Get matrix
  get <- function() x
  
  ## Update inverse cache
  setInverse <- function(inv) cachedInverse <<- inv
  
  ## Get cached inverse
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return inverse of the matrix, defined by makeCacheMatrix function.
## Once calculated, the value is stored in cache.
cacheSolve <- function(x, ...) {
  
  ## Try to get inverse from cache
  inv <- x$getInverse()
  
  ## Check if it is null
  if(!is.null(inv)) {
    ## Return cached value
    message("getting cached data")
    return(inv)
  }
  ## Cache is empty, calculate the value.
  data <- x$get()
  inv <- solve(data, ...)
  ## Save it in the cache
  x$setInverse(inv)
  inv
}
