##Together, these two functions will take the inverse of a matrix. Because of the 
##first function that stores the matrix's inverse in a cache, the second function
##doesn't have to invert the matrix from scratch if the first function has already
##done it.

##The first function creates a special matrix so we can cache its inverse. It sets the 
##value of the special matrix, gets the matrix's value, inverses the matrix,
##and then gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
       x <<- y
       m <<- NULL
     }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
                  setinverse = setinverse,
                  getinverse = getinverse)
 }

##The second function checks to see if the matrix has already been inverted. If the matrix
##was already inverted and hasn't changed, it gets the inverse from the cache 
##and doesn't re-do the calculation. Otherewise it inverts the special matrix 
##created by the above function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##To make it work, let's say you have MatrixA, 
#which you can make by: > MatrixA <- matrix(1:4, nrow=2, ncol=2)
#You'd type in:
#>z <- makeCacheMatrix(MatrixA)
#>cacheSolve(z)
#And the output would be your inverted MatrixA
