# www.coursera.org
# R Programming
# Peer Assessments
# Programming Assignment 2: Lexical Scoping
# Courserian : Benjamin Berhault (https://github.com/BenjaminBerhault)

# Example usage:
# > x <- matrix(rnorm(9), nrow = 3) // Create a matrix x
# > myMat <- makeCacheMatrix(x)     // Create a special "matrix"
# > myMat$get()                     // Return the matrix
# > cacheSolve(myMat)               // Return the inverse
# > cacheSolve(myMat)               // Call the 2nd time, so return the cached inverse

################################
# Methods:
#   set: Setter for the matrix
#   get: Getter for the matrix
#   setinv: Setter for the inverse
#   getinv: Getter for the inverse
#
# Returns:
#   This function creates a special "matrix" object that can 
#   cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  set <- function(newMat) {
    mat <<- newMat
    inv <<- NULL
  }

  get <- function() mat
  
  setinv <- function(inverse) inv <<- inverse

  getinv <- function() inv
  
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


################################
# Returns:
#   This function computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix above. If the inverse has already 
#   been calculated (and the matrix has not changed), then the 
#   cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- mat$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  mat$setinv(inv)
  
  # Return it
  inv
}
