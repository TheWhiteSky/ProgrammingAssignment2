## Assignment with functions that a) cache the inverse of a matrix
## and b) gets its inverse
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(1, 2, 7, 9), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,] -1.8  1.4
## [2,]  0.4 -0.2


# a) makeCacheMatrix - function that returns a list of functions
# Purpose - to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## b) cacheSolve - Function that calculates the inverse of the
## special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}