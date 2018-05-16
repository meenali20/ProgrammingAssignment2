##makeVector will create a matrix, Set its value, get its value. 
## it will set and get the value of inverse calculated by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve will create a function that will recall cached inverse from
## memory if it already exists else it will calculate the inverse by Solve() 
## funciton for a new matrix


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
       ## Return a matrix that is the inverse of 'x'
}
