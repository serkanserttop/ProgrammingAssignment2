#Creates a scoped environment that returns 4 functions and enables caching for the matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
  #inverse is not calculated in the initialization, hence it is set to NULL
  inverse <- NULL
  set <- function(newMat) {
    mat <<- newMat
    #invalidate cache of inverse upon setting new matrix
    inverse <<- NULL
  }
  get <- function() mat
  setInverse <- function(solved) inverse <<- solved
  getInverse <- function() mat
  #return 4 functions scoped in this environment
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- mat$getInverse()
  #we check if our inverse is in the cache
  if(!is.null(inverse)) {
    message("getting cached data")
    #if it is in the cache, just return it
    return(inverse)
  }
  #else, we need to solve it
  data <- mat$get()
  inverse <- solve(data, ...)
  #and then set the inverse cache with the new solution
  mat$setInverse(inverse)
  inverse
}
