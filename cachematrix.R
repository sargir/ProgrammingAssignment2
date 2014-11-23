## two functions below can be used to calculate and cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is really a list containing functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix inverse
## - get the value of the matrix inverse


## makeCacheMatrix creates an object of type 'list' which stores the value of 
## the matrix it is passed, cached value of its inverse, and the four
## founctions for accessing these values.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize the local inverse value to NULL
  set <- function(y) { 
    x <<- y # set the x (local to makeCacheMatrix) to y value
    inv <<- NULL # reset the 'inv' (local to makeCacheMatrix) to NULL every time 'set' is called
  }
  get <- function() x # return local x value
  setinverse <- function(inverse) inv <<- inverse # set local 'inv' to 'inverse' value
  getinverse <- function() inv # return local 'inv'
  # return a list of four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

  
## cacheSolve calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # access the inverse cached in 'x'
  # if the cached is not null return its value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise access the value of 'x', compute its inverse, cache it, and return
  data <- x$get() # access value of 'x'
  inv <- solve(data, ...) # calculate the inverse of 'x'
  x$setinverse(inv) # cache the inverse
  inv   # retrun the inverse
}
