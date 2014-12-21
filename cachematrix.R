## Using output of "makeCacheMatrix" as an input for "cacheSolve" function returns the inverse of the matrix input
## in the former. 
## To make use of the cacheing ability of "makeCacheMatrix", the functions should be executed in two seperate steps. 
## i.e., by assigning the output of "makeCacheMatrix" to a variable in step 1 and inputting the assigned variable 
## in "cacheSolve" function in step 2. the cached result will be returned upon re-executing "cacheSolve" function 
## without altering the assigned variable(neither re-assigning second element of the list nor re-executing the 
## "makeCacheMatrix" function).
## "makeCacheMatrix" function returns a list with 4 elements which are functions 
## (element 1: setmatrix function:- useful in re-assigning the input matrix and resetting the 
## getinverse function output to "null" without executing the "makeCacheMatrix"
## element 2: setinverse function: useful in re-assigning the getinverse function without running cacheSolve function 
## but should not be executed seperately as it will lead to incorrect results in cacheSolve function as the cache will be 
## refreshed manually)

## Below function outputs a list of 4 interdependent functions when a matrix is input

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## below function uses the list created in above function as input and outputs cached inverse(if exists) or updates
## the getinverse function in the list by executing setinverse function(hence cacheing the inverse of the corresponding)
## matrix. 


cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)
  x$setinverse(m)
  m
}
