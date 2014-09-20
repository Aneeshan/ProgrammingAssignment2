## The functions makes it easier to calculate the inverse of a matrix
## A matrix whose inverse is calculated is stored in a cache. So that the next
## time when the matrix is passed it is retrieved from the cache instead of calculating
## again. This saves time in calculating inverse of matrix

## The function makeCacheMatrix creates a matrix object for getting and setting 
## the values of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #cache m is set to NULL
  set <- function(y) {
    x <<- y #set the input matrix y to x
    m <<- NULL # m is set to NULL
  }
  
  get <- function() x # returns matrix x
  setinverse <- function(inverse) m <<- inverse # cache m is set to inverse of matrix x
  getinverse <- function() m #gets cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the matrix created using the function
## above. It first checks if the matrix inverse is already calculated. If yes, then 
## the calculation is not performed and gets it from cache. Else, using the 
## setinverse() function the matrix inverse is calculated.
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) { #if m is not null get the inverse matrix from cache
    message("get cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) 
  m
  
}
