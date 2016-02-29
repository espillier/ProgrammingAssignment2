## function makeCacheMatrix
##
## input : an invertible matrix
## output : a list of function-type objects
##
## processing : the function sets a memory location (m) with the matrix (x) received in input 
##     and creates a list of function objects that can be called from solveMatrix.
##
## function objects 'set' and 'get' store the original matrix received in input into a global memory location 
##    and make it available to subsequent processing by cacheSolve
## function objects 'setInvMtx' and 'getInvMtx' allow for caching and reading from cache 
##    the inverted matrix as calculated by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMtx <- function(invMtx) m <<- invMtx
  getInvMtx <- function() m
  list(set = set, get = get,
       setInvMtx = setInvMtx,
       getInvMtx = getInvMtx)

}


## function cacheSolve
##
## input : the list of function objects created by the function makeCacheMatrix
## output : the inverse of the matrix used as input for makeCacheMatrix
##
## processing : the function checks if a cached version of the inverse matrix exist.
##   If found, the function prints the message "getting cached data" and returns the inverted matrix.  Processing stops.
##   If not found, the function reads the original matrix from the memory location where it was stored by makeCacheMatrix,
##     calculates the inverse (by using the function 'solve'), sets the cache memory with the inverted matrix through a call
##     to the appropriate function in makeCacheMatrix, returns the calculated inverse and exits the function

cacheSolve <- function(x, ...) {
  m <- x$getInvMtx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMtx(m)
  m

}
