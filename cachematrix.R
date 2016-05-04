## Put comments here that give an overall description of what your
## functions do
## this functions creates a matrix and functions to calc its inverse

## Write a short comment describing this function
## makeCacheMatrix() creates a special matrix 
## that contains a list of functions to get, set a matrix 
## and getsolve and setsolve gets/sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
## cacheSolve look if inverse of the matrix was previously calculated and returns it.
## if not, calculates it and returns 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached solve matrix")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
