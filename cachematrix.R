## An adaptation to matrices/inverse of the vector/mean functions in CacheMean
## John J. Mikucki, 2014-07-23
## 
## Structure shamelessly stolen from examples in Peng's README.md,
## refined a bit of my own design.


## Apparently R doesn't do OOP, so we cobble together what amounts to a function-pointer table.  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # where we memoize the inversion result

  # function pointer: set
  # "henceforth compute over this matrix
  setfn <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL # yesterday's inverse not applicable to today's matrix...
  }
  
  # accessor for our matrix
  getfn <- function() { x }

  # accessor for the matrix's inverse
  getinversefn <- function() { inverse }
  
  # this desperately wants to be a private method
  # Better yet, both this and the compute logic should be inside the 'get' logic
  setinversefn <- function(inv) { inverse <<- inv } # god this is so bad
  
  # build our function-pointer table.  The wierd 'x=x' syntax is because we're naming the functionpointers...
  list(set = setfn, 
       get = getfn,
       setinverse = setinversefn,
       getinverse = getinversefn)
}


## This is terrible.  The memoization should take place in makeCacheMatrix' get() method
## first
## 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
