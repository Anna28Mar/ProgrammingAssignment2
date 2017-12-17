## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z)  { ##set the value of the matrix
    x <<- z
    inv <<- NULL
  }
  get <- function() x ##get the value of the matrix
  setinverse <- function(inverse) ##set the value of the inverse of the matrix 
  inv <<- inverse 
  getinverse <- function() inv ##get the value of the inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##creates a list
}


## Write a short comment describing this function
## The cacheSolve function returns the inverse of a square matrix.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
