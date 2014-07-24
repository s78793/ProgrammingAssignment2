## In this function we cache the inverse of a matrix rather than compute it repeatedly

## The function makeCacheMatrix creates a special "matrix", which:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function cacheSolve first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache else it computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
