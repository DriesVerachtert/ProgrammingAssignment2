## Create matrixes that can cache their inverse:
## makeCacheMatrix creates a new wrapper for a matrix of which the inverse will be cached.
## cachedMatrix$set() should be used to set the actual matrix.
## cachedMatrix.get() can be used to fetch the current matrix.
## cacheSolve(cachedMatrix) returns the inverse of the matrix. The inverse matrix is cached.
## Note that this only works for matrices that are invertible, see https://en.wikipedia.org/wiki/Invertible_matrix

## Return a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of a matrix. The inverse is cached, so consecutive calls without changing the matrix will return the same cached value.

cacheSolve <- function(x, ...) {
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

showSample <- function() {
  m = makeCacheMatrix()
  m$set(matrix(c(1,2,3,2,3,2,3,2,1), nrow=3, ncol=3))
  print("matrix:"); print(m$get())
  i = cacheSolve(m)
  print("inverse:"); print(i)
  i2 = cacheSolve(m)
  print("cached inverse:"); print(i2)
}
