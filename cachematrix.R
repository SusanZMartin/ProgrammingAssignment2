## These two functions 1) create the inversion of a matrix and cache this inversion
## (makeCacheMatrix) and 2) check to see if the inversion has been calculated and if
## so return the inversion value from the cache (cacheSolve). In addition the cacheSolve
## function checks to see if the matrix has changed from when the inversion in the cache
## was produced and notifies the user if this is the case

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function requires that you put in the 'special' object that you created using
## makeCacheMatrix as your first argument and a matrix as your second argument - if you
## put in a DIFFERENT matrix from the one you submitted to makeCacheMatrix you will 
## receive a message "Matrix has changed". If you put in the SAME matrix as your second 
## argument the first time you run cacheSolve you will see the inversion of the matrix, 
## if you run cacheSolve again you will see the message 'getting cached data' and then 
## the inversion (which has been retrieved from the cache)

cacheSolve <- function(x, z = matrix(), ...) {
      
      data <- x$get()
      
      if(!identical(data, z))
      {print("Matrix has changed")}
      else
      {
            
            inverse <- x$getinverse()
            if(!is.null(inverse)) {
                  message("getting cached data")
                  return(inverse)
            }
            data <- x$get()
            inverse <- solve(data, ...)
            x$setinverse(inverse)
            inverse
      }
      
      
      
}
