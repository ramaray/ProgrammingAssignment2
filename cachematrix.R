## The below functions create a matrix and put it and it's inverse in a cache
## by creating a list. If the matrix is already in the cache, it's inverse that is stored
## is displayed. If the matrix is not in the cache, it's inverse is computed
## and stored in the cache, thereby reducing compautational time incase it has to be
## recalculated.

## mackeCacheMatrix takes in a matrix as an argument. It creates a list/vector 
## to get/set/getinverse()/setinverse() of the matrix passed in as the argument.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inverse of the matrix that is passed to it as the argument.
## If the matrix inverse has been computed previously(cached), it displays 'getting cached data' 
## before returning it's value. If it is not in the cache, it computes the inverse and
## displays it.

cacheSolve <- function(x) {
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
