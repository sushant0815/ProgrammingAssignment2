## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object which is a list containing a function to a) set the value of matrix, b) get the value of matrix, c) set the value of inverse and
## d) get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function calculate the inverse of matrix object x. However, it first checks if the inverse is already calculated. If so, it gets the inverse from cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("Reading matrix inverse from cached data")
    return(i)
  }
  y <- solve(x$get())
  x$setinv(y)
  y
}
