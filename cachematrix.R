## This is a pair of functions that cache the inverse of a matrix


## It creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initializing the inverse property
  i <- NULL
  
  ## Here is the method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## And the method the get the matrix is
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Now, we get the method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computing the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Then, returning to a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## So, returning to the inverse if it is now set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Getting the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Setting the inverse to the object
  x$setInverse(m)
  
  ## Returning to the matrix
  m
}


