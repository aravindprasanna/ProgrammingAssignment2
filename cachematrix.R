## This assignment involved the writing of two functions tha caches
## the inverse of a matrix. The objective is to demonstrate the 
## usefulness of exploiting  lexical scoping to to save and restore 
## data that involves significant system compute and does not change 
## frequently using R objects.

## Function 1
## Create a "special" CacheMatrix Object that can upon instantiation
## contain
## 1) A set method that can store a matrix A in cache.
## 2) A get method that can retreive the matrix A.
## 3) a setinverse method that can set the inverse of matrix A in cache,
## 4) a getinverse method that returns the inverse of matrix A from cache.
## Caveats
## This takes only a square matrix for an input. 
## The setinverse method checks if input is a matrix but does not check for inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## invalidate the cache
  set <- function(y) { ## set functin - sets the value of the matrix in cache
    x <<- y ## matrix value is stored.
    m <<- NULL ## invalidate any existing inverse in the cache.
  }
  get <- function() x ## get the value of the matrix 
  setinverse <- function(z) { 
    if(is.matrix(z)) ## only allow matrix as input
      m <<- z ## set the inverse of the matrix
    else
      print("Error: Enter a matrix")
  }
  getinverse <- function() m ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## return the special object -- constructor
}

## Function 2
## Take input as special CacheMatrix Object
## Obtain the inverse from cache if it exists. else calculate inverse
cacheSolve <- function(x) {
  m <- x$getinverse() ## get inverse
  if(!is.null(m)) { ## does inverse exist ?
    message("getting cached data") ## Yes !! Notify that exists in cache
    return(m) ## return the cache value
  }
  data <- x$get() ## otherwuse, get the matrix value
  m <- solve(data) ## calculate inverse.
  x$setinverse(m) ## store it in cache.
  m ## return the inverse
}