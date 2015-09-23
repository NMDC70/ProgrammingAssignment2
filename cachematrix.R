## write a pair of functions that cache the inverse of a matrix
##--------
## picking up from the example given for the assignment, the steps for the solution program
## are replicated in the same manner, with the same decription. The working of the 
## program is explained at the bottom.
##=======

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse
## Thefunction, `makeCacheMatrix` creates a special "vector", which is a list containing a function to
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse matrix
##4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
## `cacheSolve`: This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix`above. If the inverse has already been calculated (and the matrix has 
##  not changed), then`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  }
## HOW THE SOLUTION COMES TOGETHER
## First time we run commond like:
##> a <- makeCacheMatrix (matrix(6:9, nrow = 2, ncol = 2))
## m is programatically set to null. When cacheSolve(a) is called the inverse is calculated
## This inverse is now stored in m. Everytime now when we call cacheSolve(a) the inverse stored 
## in m is returned without having to compute the inverse. 
## if a new matrix is assigned to makeCacheMatrix, m is reset to null & so when cacheSolve(a) is 
## called it will again computed the inverse and assigned it to m.
## a$setmatrix will set an inverse value which will alwayed be returned by cacheSolve as the value
## from a$setmatrix is assigned to m. But whenever a new matrix is created with makeCacheMatrix the
## value of m is again set to null & the new inverse is calculated by cacheSolve and assigned to m.