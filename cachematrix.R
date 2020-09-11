## Put comments here that give an overall description of what your
## functions do

## First, sets object m to be a null object.  This is our cache.
## Next the function begins listing the functions that comprise the list.
## First, make assigns the input to an object to be used by other functions.
## Getmatrix retrieves the matrix so that it may be assigned in the cacheSolve
## setInv and getInv do the same but for the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  make <- function(y) {
    x <- y
    m <<- NULL
  }
  getmatrix <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(make = make, getmatrix = getmatrix,
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data<- x$getmatrix()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
