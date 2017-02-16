## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL  
  set <- function(y) {
    x <<- y
    mat <<- NULL
    
  } 
  get <- function() x
  setinverse <- function(solve) mat <<- solve 
  getinverse <- function() mat 
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) ## returns the special 
## vector containing all the fuctions just defined

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
    
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
        ## Return a matrix that is the inverse of 'x'
}
