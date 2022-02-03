## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and returns a list with methods to get and store
## the matrix as well as its inverse. Note, it does not actually compute the 
## inverse, so it is useless wihout cacheSolve(). Running this function will 
## clear out any cached matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function takes the list provided from makeCacheMatrix, and checks to 
## see whether it already has a cached inverse. If so, it returns that inverse,
## if not, it computes and returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
