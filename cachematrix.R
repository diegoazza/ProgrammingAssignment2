## Put comments here that give an overall description of what your
## functions do

# function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #reset the inverse matrix
  im <- NULL
  
  #set matrix
  setMatrix <- function(matrix) {
    m <<- matrix
    im <<- NULL
  }
  
  #get matrix
  getMatrix <- function() {
    #return matrix
    return(m)
  }
  
  #set the inverse of matrix
  setInverse <- function(inverse) {
    im <<- inverse
  }
  
  #get the inverse of matrix
  getInverse <- function() {
    return(im)
  }
}


## Write a short comment describing this function

# function to compute the inverse of the "special matrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m2 <- x$getInverse()
  
  #return the inverse if the cache is existing
  if(!is.null(m2)) {
    message("Get inverse from cache")
    return(m2)
  }
  
  
  #get matrix from special matrix
  m1 <- x$getMatrix()
  
  #calculate the inverse using matrix multiplication
  m2 <- solve(m1) %*% m1
  
  #set the inverse to special matrix
  x$setInverse(m2)
  
  #return the result
  return(m2)
}
