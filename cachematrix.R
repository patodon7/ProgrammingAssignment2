## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function is intended to have the following functionalities
## 1 - introduce a matrix
## 2 - return an special vector which is a list containing functions to
## a. Set the the matrix
## b. Get the the matrix
## c. Get the inverse of the matrix
## d. Set the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  ## At first the matrix should be created as null
  inv <- NULL
  
  #Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get should return the matrix
  get <- function(){
    x
  }
  
  # setinv sets the inv variable
  # This should be used only with cacheSolve
  setinv <- function(i){
    inv <<- i
  }
  
  # return the inverse matrix chached
  getinv <- function() {
    inv
  }
  
  # return the special vector
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

## This function takes the special vector returned by makeCacheMatrix
## and calculate the inverse matrix of x if it is not cached
## if it is already cached retrieve the one from the cache.

cacheSolve <- function(x, ...) {
  # Getting the cached inverse if exists
  inv <- x$getinv()
  
  # First checking if inv is null
  if (is.null(inv)){
    #if inv is null, we have to calculate the inverse
    matrix <- x$get()
    matrix
    inv <- solve(matrix, ...)
    x$setinv(inv)
    
  } else {
    
    message("The inverse is already cached! Returning it.")

  }
  
  return(inv)
}

## Examples:
## example1 <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),3,3))
## cacheSolve(example1)
## cacheSolve(example1) ## To check if the caching works!
## example2 <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(example2)
## cacheSolve(example2) ## To check if the caching works!