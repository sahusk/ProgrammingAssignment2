## The following functions work in synergy to calculate inverse of a given Matrix.
## If the inverse is again called for the same matrix , then output is given out from
## the cached memory than calculating it again. 

## makeCacheMatrix function takes a matrix as input and return an object as 
# which is list of functions. These functions canbe used to set new matrix,
# get the value of existing matrix, set the inverse of the matrix and get the
# value of inverse of the matrix.

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


## cacheSolve function gets the inverse of the matrix by using solve function in R. 
## If Inverse has already been calculated then it gets the inverse from the cached memory.

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
