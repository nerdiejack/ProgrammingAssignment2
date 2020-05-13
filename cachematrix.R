## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and does:
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function () m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This functions takes the matrix object created by "makeCacheMatrix" and
## checks if inverse is already calculated. If so, it gets the inverse from
## the cache and skips computation. Otherwise the inverse of the matrix is 
## calcuated and the result  set in the the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
