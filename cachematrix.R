# makeCacheMatrix: is a function which returns a list of functions
# The get function gets the value of the matrix stored prior to
# The set function stored the matrix. So if the set function is called from 
# global environment or ano other function the matrix stored previously is
# overwritten by <<- operator. Because this operator can change the value of
# a field defined in any other environment.
# The getinverse operator returns the inverse of the original matrix. 
# If it is not set then it will return null.
# setinverse will set the inverse of the matrix with << operator.

makeCacheMatrix <- function(x = matrix()) {
    revm <- NULL
    # The following function sets the original matrix passed to it
    # Subsequently it sets the inverse "revm" to null as new matrix has been
    # passed and the inverse of the same is not set yet. 
    set <- function(y) {
    	x <<- y
	revm <<- NULL
    }
    # The get function returns the original matrix
    get <- function() x
    # setinverse function sets the inverse of the original function passed to it
    setinverse <- function(solve) revm <<- solve
    # getinverse function returns the inverse of the original function
    getinverse <- function() revm
    # The following statement returns the list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. The makeCacheMatrix returns a list of functions.
# The getinverse function which is the 4th element of the list returns the inverse
# of the matrix. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache. Otherwise it calculates the inverse and stored it
# by calling setinverse which is the 3rd element of the list returned by 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
  #revmat will contain the inverse of the matrix
  revmat <- x$getinverse()
  if(!is.null(revmat)) {
  	message("getting cached data")
	return(revmat)
  }
  # orimat will conain the original matrix
  orimat <- x$get()
  revmat <- solve(orimat, ...)
  x$setinverse(revmat)
  # the function will return the inverse of the matrix revmat
  revmat
}
