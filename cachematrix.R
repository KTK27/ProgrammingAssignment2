## These functions create a matrix inversion and cache it to decrease computation time. 

## Making a cache inverse matrix by:
##    1. setting the matrix value
##    2. getting the matrix value
##    3. setting the matrix inverse value, and 
##    4. getting the matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) INV <<- inverse
    getinverse <- function() INV
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## This function chects to see if the inverse matrix already exists, 
##    If it does exist it skips this computation
##    If it does not exist it sets the inverse matrix value.

cacheSolve <- function(x, ...) {
  INV <- x$getinverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- inverse(data, ...)
  x$setinverse(INV)
  INV
}
