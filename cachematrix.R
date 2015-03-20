## inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly
## These functions cache the inverse of a matrix

## This function does the following 4 tasks
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## It first checks if the inverse has already been computed. If so, it gets the 
## result and skips the computation. If not, it computes the inverse, sets the 
## value in the cache via setinverse function. This function assumes that the 
## matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## run example:
## > x = rbind(c(1, -1/5), c(-1/5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0

## No cache in the first run
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data...
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
