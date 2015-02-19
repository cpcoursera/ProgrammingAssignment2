## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creates a list where the initial matrix to be inversed is stored together with functions for its manipulation
## typical CRUD (Create Update Delete) operation for the list, where it is possible to set and get the initial matrix,
##together with the inverse if it is already set. What is important here is the operator <<- which where the assignment
##happens to the value that is found on the parent environment. So when we have i <<- inv inside the setinverse, the body
##of it's parent function (makeCacheMatrix) is search for i and if it is found that's the variable where the value will be assigned
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function uses the functions that are stored in the list by makeCacheMatrix and calculates the inverse of the matrix.
## it checks if the inverse is stored and if yes then it returns the cached one, otherwise it does the calculation using the 
##solve() function and stores the result using the setinverse function of the list returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
