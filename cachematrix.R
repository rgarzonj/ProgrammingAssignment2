## Put comments here that give an overall description of what your
## functions do
## "makeCacheMatrix" creates an special type of matrix that allows caching the inverse of this matrix.
## "cacheSolve" is used to generate the inverse of our special type of matrix but also caches the Inverse. 
## In later calls to cache Solve, the inverse is not calculated but returned from the cache.

## Write a short comment describing this function
## "makeCacheMatrix" creates our special type of matrix that supports
## caching the inverse matrix
## To test it we could run:
## myspecialmatrix<-makeCacheMatrix()
## myspecialmatrix$set(matrix(c(1:4),2,2))
## We can now check if the data is correctly assigned
## myspecialmatrix$get()
##

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## "cacheSolve" generates the Inverse matrix of a matrix created with "makeCacheMatrix"
## First time the Inverse is calculated, the solve function is used to cache and return the inverse.
## On later calls, the previously cached inverse matrix is returned without being computed
## We can test it by calling cacheSolve(myspecialmatrix) , where myspecialmatrix has been created with 'makeCacheMatrix'
## First time will return the inverse.
## if we call again cacheSolve(myspecialmatrix), the cached value is returned with the message "getting cached data"
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
  i
}
