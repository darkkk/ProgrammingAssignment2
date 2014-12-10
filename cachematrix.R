## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix:
## function that takes as a parameter ordinary matrix (assuming invertible)
## and creates "special" matrix class (technically creates environment with 4 functions)
## that can store the matrix inverse.
## class is created with makeCacheMatrix(someMatrix) and has 4 methods
##    - get() --- returns original matrix
##    - set(someOthermatrix) --- into object stores someothermatrix
##    - setinv(inverse)   --- stores the elsewhere calculated inverse of the matrix in class
##    - getinv() --- returns the currently stored inverse
## cacheInverse(specialmatrixclass) is function to calculate (and store, if necessary) the inverse of the matrix in class



## Write a short comment describing this function
## creates special matrix class with 4 methods, described above
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y;    ##assigns value y into variable 
      ##named x that lives in environment made by fn. call makeCacheMatrix
    inv <<- NULL;
  }
  get <- function() return(x)  ##returns value under name x from within fn. call environment
  setinv <- function(calculatedInv) inv <<- calculatedInv;
  getinv <- function() return(inv);
  l = list(
    set = set, get = get, 
    setinv = setinv, getinv = getinv
    );
  return(l)
  
}


## Write a short comment describing this function
## function to return special matrix class inverse - calculates & stores if necessary, 
## otherwise just return already calculated inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)    
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  return(inv)
  
  
}
