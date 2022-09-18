# Functions to cache matrix inversions:
#   
#   Calling cacheSolve() with a matrix as a parameter will
#   return the parameter inverted.
#   
#   Calling makeCacheMatrix() on the matrix will allow the
#   inverted result to be inverted.
#   Any repeat calls will return the cached matrix.
#   
#   Stored data may be accessed directly via
#   $set(), $get(), $setinv(), and $getinv.



## makeCacheMatrix adds functions to allow caching of the result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the invers of 'x', taking advantage of
## caching functions if made available via makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

    m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


# Example results:
# > m1<-matrix(c(.5,-1,-.25,.75),2,2)
# > myMatrix_object <- makeCacheMatrix(m1)
# > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    6    2
# [2,]    8    4
# > cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    6    2
# [2,]    8    4

