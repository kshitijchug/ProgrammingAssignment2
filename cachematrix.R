## Put comments here that give an overall description of what your
## functions do

## This function takes an matrix as an argument

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL #i is the inverse matrix
  setMatrix <- function(n) {
    m <<- n
    i <<- NULL
  }
  getMatrix <- function() m
  setInvMatrix <- function(invMatrixVar) i <<- invMatrixVar
  getInvMatrix <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## This function will take an object of the makeCacheMatrix function argument

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  i <- m$getInvMatrix() # i is the inverse matrix
  if(!is.null(i)) {
    message("getting cached data for inverse matrix")
    return(i)
  }
  data <- m$getMatrix()
  i <- solve(data, ...) # formula to inverse a matrix
  m$setInvMatrix(i)
  i

}
