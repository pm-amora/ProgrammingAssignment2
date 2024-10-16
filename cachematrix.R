## The first function, makeCacheMatrix creates a special "matrix" object that is able to cache its inverse matrix

# set the value of the matrix (set)

# get the value of the matrix (get)

# set the value of the cached inverse matrix (setInverse)

# get the cached inverse matrix (getInverse)

## The second function (cacheSolve) calculates the inverse of the matrix created by makeCacheMatrix. If the inverse has already been computed and the matrix has not changed, it retrieves the cached inverse. 
## Otherwise, it generates the inverse using the solve function and caches it.

makeCacheMatrix <- function(x = matrix()) {
  #set the value of inverse to NULL
  inv <- NULL
  #set the matrix and clear any cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  
    get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  #function to compute an inverse of the special matrix created by makeCacheMatrix
  
  cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    # If the inverse is not cached, calculate it
    mat <- x$get()
    inv <- cache(mat, ...)
    # Cache the inverse
    x$setInverse(inv)
    return(inv)
  }

## cacheSolve will compute the inverse of mat and store it in the cache. The second call will return the cached inverse, avoiding the need to recompute it.
