makeCacheMatrix <- function(p = matrix()) {
  ##  1. set the matrix
  ##  2. get the matrix
  ##  3. set the inverse
  ##  4. get the inverse
  inv = NULL
  set = function(q){
    p <<- q
    inv <<- NULL
  }
  get <- function() p
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}





## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(p, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- p$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- p$get()
  inv <- solve(mat, ...)
  p$setInverse(inv)
  inv
}
