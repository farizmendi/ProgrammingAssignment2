## Put comments here that give an overall description of what your
## functions do
## This is a program that provides an inverse of a matirx and keeps it as cache to avoid reacalculation

## Write a short comment describing this function

## Prepares the matrix to be used as input for the function cacheSove
makeCacheMatrix <- function(x = matrix()) {m <- NULL
                                           set <- function(y) {
                                             x <<- y
                                             m <<- NULL
                                           }
                                           get <- function() x
                                           setinverse <- function(inverse) m <<- inverse
                                           getinverse <- function() m
                                           list(set = set, get = get,
                                                setinverse = setinverse,
                                                getinverse = getinverse)
}


## Write a short comment describing this function
## It finds the inverse of the matrix
cacheSolve <- function(x) { m <- x$getinverse()
                                 if(!is.null(m)) {
                                   message("getting cached data")
                                   return(m)
                                 }
                                 data <- x$get()
                                 m <- solve(data)
                                 x$setinverse(m)
                                 m
  
  ## Return a matrix that is the inverse of 'x'
}
