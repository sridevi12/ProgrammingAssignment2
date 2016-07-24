## Programminsg assignment 2 - has 2 functions - makeCacheMatrix and cacheSolve. 



## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  m <-NULL
  set <- function(y = matrix()){
     x <<- y
     m <<- NULL
  }

  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix object created by makeCacheMatrix
## it first checks if the inverse has already been computed and is present in cache. If yes, it will
## retrieve it from cache. Else, it will compute the inverse.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
