## R functions for Assignment 2
## Adapted from examples ("makeVector" and "cachemean") in the instructions

## makeCacheMatrix creates a list-structure of functions for a specific matrix
## Per instructions, matrix is assumed to be square and invertable, no checking
## x$set stores the original matrix, and sets its inverse to NULL
## x$get retrieves the original matrix
## x$setinv causes the inverse (calculated externally) to be stored
## x$getinv retrieves the inverse
## x and inv are global variables

 makeCacheMatrix <- function(x=matrix()) {
      inv<-NULL
  
      set<-function(y=matrix()){
          x<<-y
          inv<<-NULL
      }
 
      get<-function() x
 
      setinv<-function(calcinv) inv <<- calcinv
 
      getinv<-function() inv
  
      list (set=set, get=get, setinv=setinv, getinv=getinv)
 
 }

## cacheSolve invokes the previously created "makeCacheMatrix" functions
## If the inverse has already been calculated (x$getinv is non-NULL), use it
## Otherwise, retrieve the matrix via x$get, calculate the inverse, store it via x$setinv

cacheSolve <- function (x, ...) {

    inv<-x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data<-x$get()
    inv<-solve(data)
    x$setinv(inv)
    inv
}
