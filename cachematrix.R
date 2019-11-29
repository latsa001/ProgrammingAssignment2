## These functions will allow us to be able to cache what could be time wasting computations. 
## We cache values so that when we need them in the future, we can look them up in the cache vs re-running them.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  ## Create a variable m as a matrix.
  ## return: list with functions that
  ##              1 set a matrix
  ##              2 get that matrix
  ##              3 set the inverse
  ##              4 get that inverse
  ##         cachesolve() will use this output
  
  inver = NULL
  set = function(x) {
    m <<- x
    inver <<- NULL
  }
  get = function() m
  setinver = function(inverse) inver <<- inverse 
  getinver = function() inver
  list(set=set, get=get, 
       setinver=setinver, 
       getinver=getinver)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  
  inver = x$getinver()
  
  if (!is.null(inver)){

    message("retreiving cached data")
    return(inver)
  }
  
  datamatrix = x$get()
  inver = solve(datamatrix, ...)
  x$setinver(inver)
  
  inver
}

