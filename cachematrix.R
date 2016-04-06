## test case. Create a matrix that I can solve for inverse. 
B = matrix( 
  c(2, 4, 3, 1), 
  nrow=2, 
  ncol=2
) 

## make a cache of matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## solve matrix inverse if not already cached
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