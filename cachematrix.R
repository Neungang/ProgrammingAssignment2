## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## sets the inverse to NULL as a place holder for the future value
##  the following function defines a function to set the matrix x to a new matrix y and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  } 
  get <- function() x ## returns the matrix x
  setinverse <- function(solve) m <<- solve ## sets the matrix m to solve
  getinverse <- function() m ## returns the inverse m
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) ## returns the special 
## vector containing all the fuctions just defined

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
