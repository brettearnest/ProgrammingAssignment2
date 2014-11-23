#################################################################
## The following functions are intended to provide a more
## computationally efficient means of retrieving data from
## a matrix when the calculations have already been performed
## and the argument constituents have not been altered since
## the most recent 'set' execution.

#################################################################
## This function is designed to create a matrix object which
## can then be instantiated. To "instantiate" is to create
## a working copy of the object itself. This instantiated
## object can then be manipulated without disturbing the
## original structure of the original object itself,
## sometimes called a 'class'.
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL

   # Create a set function and move data into
   # the current environment from
   set <- function(y) {
      # This <<- operator is an assignment to the
      # value x in the nearest enclosing environment
      # in which it is found, replacing that value of
      # x with the local value y in this function.
      # Null assingment afterwards is the same concept.
      x <<- y
      inv <<- NULL
   }

   # Here the lexical scoping in R gives x the value
   # of the parameter in makeCacheMatrix, since it is
   # the closest in scope.
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv

   # Now return a list with the four functions included
   # in this function; more like a class/method relationship.
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#################################################################
## This function is specifically designed to save computing
## cycles when recalculation is unnecessary. It checks for
## evidence of a previously-set matrix. If it finds the data
## has already been set, just return the data; otherwise,
## do whatever calculations are necessary to set it into
## a cached state.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()

   # Check to see if there are any values in the
   # matrix. If , proceed to get
   if(is.null(inv)) {
      # The matrix is null. Do the calculations.
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
   }
   else {
      # The matrix is not null. Just return
      # the already-existing data.
      message("Retrieving Cached Data.")
      return(inv)
   }
}
