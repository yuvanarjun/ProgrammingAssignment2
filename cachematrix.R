## In general, 'makeCacheMatrix' function creates a matrix from the matrix that is passed as an argument
## and returns a list of functions. This list can be used to perform functions like setting a matrix, returning/getting
## a matrix, setting an inverse of the matrix and returing/getting the inverse of the matrix.

## On the other hand, the 'cacheSolve' function returns the inverse of the matrix at a higher level. At the lower level,
## returns the inverse from the cache, if previously computed. If not, it calculates afresh.

## Both these functions are complementary to each other. That is, both these functions are to be used in order to compute
## and return the value of the input matrix.



## 'makeCacheMatrix' function
## This function takes in a square matrix as an argument and returns a list of 4 functions
## The functions returned in the list are set, get, setinverse and getinverse
## 'set' function: sets the value of the matrix from the input matrix passed as the argument and resets 
##  the inverse value to NULL
## 'get' function: returns the value of the matrix stored when called
## 'setinverse' function: sets the inverse of the matrix when the value (as matrix) is passed to it as an argument
## 'getinverse' function: returns the inverse of the matrix which is stored, by default NULL is stored as the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' function
## This function takes in an object, which is a list of functions (previously created using 'makeCacheMatrix' function),
## amongst other arguments and returns the inverse of the matrix which is stored in the object.
## When this function is called, it invokes the 'getinverse' function from the object passed to it and stores the value of 
## the inverse that is returned upon calling the function.
## This inverse is checked to see if it is a NULL value or if the inverse has been previously computed and stored.
## If previously computed, it returns the cached (stored) value and if not it freshly computes the inverse of the matrix,
## sets this inverse by invoking the 'setinverse' function from the object and then returns the inverse.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
