## Since the computing the inverse of a large matrix can be time
## consuming, the functions below have been created to allow the 
## inverse of a matrix to be cached once calculated.  Subsequent
## calls to the 'cachesolve' function will first look for a 
## cached version.  If one exists it is loaded otherwise the
## full computation will occur.

B = matrix(
  c(4, 3, 3, 2), 
  nrow=2, 
  ncol=2) 

## This function is modeled after the makeVector function
## created by Roger Peng.  The function takes in a matrix
## then creates function for getting/setting a matrix
## and getting/setting the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function is modeled after the cachemane function
## created by Roger Peng.  It takes in the cached matrix
## returned by the makeCacheMatrix function.  If the inverse
## has already been computed then the function returns the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
