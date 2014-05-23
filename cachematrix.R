## These two functions allow you to record a base matrix and its inverse

## This function creates a list which holds a base matrix and its inverse
## Output: A list object containing getter and setter functions for base matrix and inverse

makeCacheMatrix <- function(x = matrix()) {

  # inv holds the inverse; initially set to null
  inv <- NULL
  
  # Setter for base matrix, which is the base matrix and null the inverse since x has been reset
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter for base matrix
  get <- function() x
  
  # Setter for inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Getter for inverse
  getinverse <- function() inv
  
  # List to hold functions; this is basically like a class
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function checks and calculates inverse of matrix x
## Output: inverse of x (cached if available or calculated, stored and returned if not)

cacheSolve <- function(x, ...) {
  
  # Get inverse to check if it has been created
  inv <- x$getinverse()
  
  # Check if inverse exists (is not null) and return if so
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Inverse must have been null; retrieve base matrix then calculate, store, return inverse
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
