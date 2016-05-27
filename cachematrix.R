## This function creates alist to set and get the values of vector and Inverted Matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL      #set the value null.
  set <- function(y){  # a functio to set x with a new value y and resets value of inverse to Null.
    x <<- y
    inverse <<- NULL
  
  }
  get <- function() x     # function to return the value of x
  setInverse <- function(invmat) inverse <<- invmat     # function to set the inverted matrix to a vector.
  getInverse <- function() inverse      # function to return the value of inverse.
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


## This function inverts the matrix.If it is already inverted then it gives the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
       ##checks to see if the value is already calculated.If it is then it gives the cached value.
  if(!is.null(inverse)){
    message("chached data")
    return(inverse)
  }
  
        ##if the value is not already calculated then it is calculted here.
  mat <- x$get()
  inverse <- solve(mat,...)
  x$setInverse(inverse)
  inverse
}
