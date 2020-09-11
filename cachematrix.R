## This script makes two functions, makeCacheMatrix and cacheSolve
## The first function generates a list of functions to be used
## by cacheSolve, including the function that assigns the returned
## value from cacheSolve to an object in the parent environment. 

## Second, the cacheSolve function finds the inverse of the matrix we input
## into the makeCacheMatrix function.  It either computes the inverse using the
## the solve function, or it retrieves the cached value if it exists.

## First, sets object m to be a null object.  This is our cached inverse.
## Next the function begins listing the functions that comprise the list.
## First, make assigns the input to an object to be used by other functions.
## If there is a cached inverse, it clears it here since we are changing matrix.
## getmatrix retrieves the matrix so that it may be assigned in the cacheSolve.
## setInv and getInv do the same but for the inverse.
## Note that setInv sets the value of m, the cached inverse, in the higher level
## environments, or parents environments.

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


## CacheSolve takes the function list created by makeCachematrix.
## First it retrieves the cached object, then checks if it exists.
## If it exists, it returns the object and tells us that it is from the cache.
## If it does not, it continues on to calculate the inverse.
## To do so, it first retrieves the original matrix from the cache.
## Then it passes the matrix to the solve function, assigning the result as m.
## Note that the function will return an error here when
## the matrix cannot be inverted.
## It then sets the resulting matrix to the cached value of m using setInv.
## Returns the inverse at the end of the function.

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
