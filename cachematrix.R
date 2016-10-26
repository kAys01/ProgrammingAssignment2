## Put comments here that give an overall description of what your
## functions do

## will cache a matrix, allowing 4 different functions to act on this object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                     
  
  set <- function(y) {
    x <<- y                     
    m <<- NULL                  
  }
  
  get <- function() x           
  setinverse <- function(solve) m <<- solve  
  getinverse <- function() m       
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #lefthand side = user given function names / righthand side = the functions
}


## returns the inverse of a square matrix (only if result is mathematically defined)
#will recompute the inverse for each new matrix given, otherwise, it will pull the cached inverse

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
}