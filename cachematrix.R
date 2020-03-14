## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Matrix object is created which can cache its inverse
makeCacheMatrix <- function(x = matrix())
  {
  a <- NULL
set <- function(y) {
  x <<- y
  a <<- NULL
}
get <- function() x
setinverse <- function(inverse) a <<- inverse
getinverse <- function() a
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Write a short comment describing this function
## inverse of given special matrix object is computed.
## inverse from cache is obtained by "cacheSolve". 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data) %*% data
  x$setinverse(a)
  a
}
