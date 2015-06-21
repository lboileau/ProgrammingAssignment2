## makeCacheMatric creates a cached matrix object with 4 methods:
## get : get the matrix vector
## set : set the matrix vector to the given input x
## getInverse : get the inverse of the matrix, first checking cache
## setInverse : set the inverse of the matrix and cache it
makeCacheMatrix <- function(sqrMatrix = matrix()) {
  inverse <- NULL
  set <- function(x) {
    sqrMatrix <<- x
    inverse <<- NULL
  }
  get <- function() sqrMatrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() {
    if(is.null(inverse)){
      message("Solving inverse...")
      setInverse(solve(sqrMatrix))
    }
    inverse
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve returns the inverse of a given cacheMatrix
cacheSolve <- function(cacheMatrix, ...) cacheMatrix$getInverse()
