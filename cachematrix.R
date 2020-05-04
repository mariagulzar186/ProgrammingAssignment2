## this function gets a matrix as an input, sets the values, takes its inverse
## using solve and sets it too
## <<- helps to send it to other other environments as well to be loaded from cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) 
    inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function gets the inverse if its already in the cache, otherwise calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.na(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}




m1 <- makeCacheMatrix(x)
  
cacheSolve(m1)

 