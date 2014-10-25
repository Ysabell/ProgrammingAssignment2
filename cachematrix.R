## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  
  mym <- NULL
  
  ## Method to set the matrix
  
  set <- function(y) {
    x <<- y
    mym <<- NULL
  }
  
  ## Method the get the matrix and return the matrix
  
  get <- function() x
  
  ## Method to set and get the inverse of the matrix
  
  setinverse <- function(solve) mym <<- solve
  getinverse <- function() mym
  
  ## Return a list of the methods
  
  list(set = set, get = get, setinverse = setinverse, getinvere = getinverse)
}


## Write a short comment describing this function

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mym <- x$getinverse()
  
  ## Return the inverse if its already set
  
  if(!is.null(mym)){
    message("getting cached data")
    return(mym)
  }
  
  ## Calculate
  
  data <- x$get()
  mym <- solve(data, ...)
  
  # Cache the inverse
  
  x$setinverse(mym)
  mym
}
