## This function is designed to determine the inverse of a given matrix.
## In the event that the inverse has already been determined, the function
## will simply retrieve the inverse from memory instead of undergoing the 
## computationally intensive inverse calculation.

## The "makeCacheMatrix" function will create a list that stores a matrix and
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
      s <- matrix(data=NA,nrow = nrow(x),ncol = ncol(x)) ## Matrix of NAs that will eventually contain inverse
      ## The set function sets the matrix x to a new value and resets the inverse matrix
      set <- function(y) {
            x <<-y
            s <<- matrix(data=NA,nrow = nrow(x),ncol = ncol(x))
      }
      ## The get function returns the current value of the matrix x
      get <- function() x
      ## The setinv function calculates the inverse of the matrix.
      setinv <- function(solve) s <<- solve
      ## The getinv function returns the current value of the inverse matrix. 
      getinv <- function () s
      ## This list stores the functions to set and get the values for both the matrix and its inverse
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function "cacheSolve" calculates the inverse of a matrix. If that matrix inverse is
## already stored in memory, the function will just retrieve it from memory.

cacheSolve <- function(x, ...) {
      s <- x$getinv()
      ## "s" is the returned value of the inverse matrix
      ## If the "s" matrix does not contain all "NAs" then the already calculated inverse is returned
      if(any(!is.na(s))) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      ## If the inverse is composed of all "NAs," then we store the value of the matrix in "data,"
      ## solve for the inverse of "data" and store it in "s"
      s <- solve(data,...)
      ## "x$setinv(s) adds the inverse matrix to the list so that it will be in memory if we need it again
      x$setinv(s)
      s
      ## The "s" inverse matrix is returned.
}
