## makeCacheMatrix is a function that creates a list
## to set and get the value of an invertible matrix,
## and set and get the value of its inverse.
## The second function, cacheSolve, calculates the
## inverse of the matrix created with makeCacheMatrix
## unless it is already calculated, in which case
## it gets the inverse value from the cache.

## This function creates a list containing a
## function to set the value of a matrix, get
## the value of a matrix, set the value of the 
## inverse of the matrix, and get the value of 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     #set the value of the matrix
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     #get the value of the matrix
     get <- function() x
     
     #set the inverse of the matrix
     setinverse <- function(inverse) inv <<- inverse
     
     #get the inverse of the matrix
     getinverse <- function() inv
     
     #create a list of functions
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function calculates the inverse of the matrix
## passed into it, unless the inverse has already
## been calculated, in which case it will set the
## inverse to the value stored in the cache and 
## return that inverse matrix value.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     
     ## If inverse has been calculated, 
     ## then return it
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     
     ## Otherwise, calculate the inverse and
     ## return it.
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
