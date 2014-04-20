## First use the makeCacheMatrix to cache the inverse of the matrix
## ex : matrix <- rbind(c(1,0,5),c(2,1,6),c(3,4,0))
## ex : x <- makeCacheMatrix(matrix)
## Then calculate the inverse of the matrix with the function cacheSolve
## ex: cacheSolve(x)

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  
  setInvMatrix <- function(matrix) { m <<- matrix }
  getInvMatrix <- function() { m }
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInvMatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

