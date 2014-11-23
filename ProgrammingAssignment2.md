## functions that cache the inverse of a matrix


## Creating a special matrix object that can cache its inverse
makeCacheMatrix = function( m = matrix() ) {
  
  ## Initialize the inverse
  i = NULL
  
  ## set the matrix
  set = function( matrix ) {
    m = matrix
    i = NULL
  }
  
  ## get the matrix
  get = function() {
    ## Return the matrix
    m
  }
  
  ## set the inverse of the matrix
  setInverse = function(inverse) {
    i = inverse
  }
  
  ## get the inverse of the matrix
  getInverse = function() {
    ## Return the inverse
    i
  }
  
  ## list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve = function(x, ...) {
  
  ## Return the inverse of 'x'
  m = x$getInverse()
  
  ## return the inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data = x$get()
  
  ## using matrix multiplication
  m = solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}