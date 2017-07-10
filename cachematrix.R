## The following functions are able to cache potentially time-consuming computations.


## The function creates a matrix, which will contain a function to set and get the value 
## of the matrix and setand get the value of the inversion.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Check:
a <- matrix(rnorm(9), 3, 3)
b <- makeCacheMatrix(a)
b$get()
cacheSolve(b)
