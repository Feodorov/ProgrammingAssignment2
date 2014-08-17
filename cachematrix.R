# Matrix with cachable inverse value
# Example: 
# Create cached matrix 
# cachedMatrix <- makeCacheMatrix(mymat)
# Output calculated inverse and cache it:
# cacheSolve(cacheMatrix)
# Output cached inverse value:
# cacheSolve(cacheMatrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ### 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ### 2. get the value of the matrix
  get <- function() x
  
  ### 3. set the value of the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  
  ### 4. get the value of the inverse of the matrix
  getsolve <- function() m
  
  # return our list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # retrieve from cache
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # create cached matrix if there is no cache
  data <- x$get()       
  m <- solve(data, ...) 
  x$setsolve(m)	      
  m		      
}
