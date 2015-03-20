## The following pairs of functions caches the inverse of a matrix

## The first function makeCacheMatric creates a special matrix
## It creates a list containing functions to 
## 1. Set a value of a matrix (set)
## 2. Get a value of a matrix (get)
## 3. Set Inverse of a matrix (setinverse)
## 4. Get the Inverse of a matrix (getinverse)
## Note that variables x and i are available in the parent environments as they are set using <<- operator

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse ## i will be available outside the setinverse function
  getinverse <- function() i ## getinverse can get the value of i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix returned by makeCacheMatrix function
## If the inverse is already calculated, do not recalculate but return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  message("not getting cached data")
  i
}