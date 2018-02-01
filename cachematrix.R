# Write a pair of functions that cache the inverse of a matrix
# Computing the inverse of a square matrix can be done with the solve function in R
# assume that the matrix supplied is always invertible (det!=0)

# makeCacheMatrix: this functions creates a special "matrix", which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse of the matrix
# 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- matrix() 
  
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <- matrix()
  }
  
  # 2.get the value of the matrix
  get <- function() x
  
  # 3.set the value of the inverse
  setinverse <- function(solve) inverse <<- solve
  
  # 4.get the value of the inverse
  getinverse <- function() inverse
  
  # create the list with all previous information
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if ( (sum(is.na(inv))==0) & (identical(x$get(),a) )) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
