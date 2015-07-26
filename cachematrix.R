## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # This stores the cached value of the matrix
  # initialize to NULL
  cache <- NULL
  
  # This will create the matrix in the current working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # Here I get the value of the matrix
  get <- function() x
  # This step will invert the matrix and store the value in cache
  setMatrix <- function(inverse) cache <<- inverse
  # This step we will get the inverted matrix from cache
  getInverse <- function() cache
  
  # This will return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function will return a matrix that is the inverse of 'x'
  
  ## GetInverse will attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  #  inverted matrix will be returned from cache if it exists
  # If inverted matrix does not exist then will create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    
    # Here it will display matrix in console
    return(cache)
  }
  
  # As matrix does not exist this tep will create the matrix
  matrix <- x$get()
  
  tryCatch( {
    # In this step we set and return inverse of matrix
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # Now set inverted matrix in cache
    x$setMatrix(cache)
  } )
  
  # Lastly display matrix in console
  return (cache)
}





