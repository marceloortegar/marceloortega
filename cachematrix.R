## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #Set the given matrix, "x" is a list "y" from now on and "m" (inverse) NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get function: Returns the matrix given with x$get
  
  get <- function() x
  
  #Set inverse function: if done, solves the inverse and asigns new value for "m", i.e. the inverse of "x"
  
  setinverse <- function(solve) m <<- solve(x)
  
  #Get inverse: returns the value for "m", i.e. inverse of "x"
  
  getinverse <- function() m
  
  #Lists all the elements
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #Checks if inverse is already done
  
  m <- x$getinverse()
  
  #If done, returns the value of the inverse of "x"
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If not, solves "x", asigns the value to "m", stores the inverse of "x" with function setinverse(), returns the inverse
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
