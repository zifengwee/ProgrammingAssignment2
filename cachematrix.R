## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #assign NULL to inv
  set <- function(y) {   #use double assignment operator
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get value of the matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv #get value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("Getting cached data") #message to inform getting cached data
    return(inv)
  }  
  data <- x$get()
  inv <- solve(data) #solve inverse of matrix
  x$setinv(inv)
  inv
}