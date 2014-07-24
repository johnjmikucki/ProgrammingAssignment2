## An adaptation to matrices/inverse of the vector/mean functions in CacheMean
## John J. Mikucki, 2014-07-23
## 
## Structure shamelessly stolen from examples in Peng's README.md,
## refined a bit of my own design.
##
## Usage:
## > mmm = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > cacheSolve(mmm)
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mmm)
## getting cached data
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

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

  # accessor for the matrix's inverse.
  # I moved this logic up from cacheSolve, which is IMHO vestigial and really belongs here-- I 
  # shouldn't need an external helper function to do my memoization here, and if R stylistically
  # wants one as syntactic sugar, let's make it obvious that's all it is.
  # 
  getinversefn <- function() { 
    # grab memoized field from parent frame
    inverse ->> temp
    
    if(is.null(temp)) {
      temp  <- solve(x)  # first time, compute 
      inverse <<- temp   # ...and memoize result    
    } else {
      message("getting cached data")
    }
    
    temp # return result
  }
  
  # build our function-pointer table.  I prefer the functions to have 'fn' suffixes so I can tell them
  # from the names we're binding to...
  list(set = setfn, 
       get = getfn,       
       getinverse = getinversefn
       #setinverse should never have been here IMHO.
      )
}


## Syntactic sugar 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$getinverse()
}
