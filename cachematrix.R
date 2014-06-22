## The following functions are used to create a special object storing a invertible matrix and 
## cache the inverse of that matrix, so that the inverse could be retrieved later


## This function is used to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

            inver <- NULL
            set <- function(y) {
                   x <<- y
                   inver <<- NULL  
            }
            get <- function()x
            setinver <- function(solve) inver <<- solve
            getinver <- function() inver
            list(set = set, get = get,
                 setinver = setinver,
                 getinver = getinver)
}


## This function is used to compute the inverse of matrix returned from 'makeCacheMatrix', or
## retrieve the inverse from the cache if it is already calculated

cacheSolve <- function(x, ...) {
          inver <- x$getinver()
          if(!is.null(inver)) {
                message("getting cached data")
                return(inver)               
          }
          matrix <- x$get()
          inver <- solve(matrix, ...)
          x$setinver(inver)
          inver
  ## Return a matrix that is the inverse of 'x'
}
