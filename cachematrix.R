## In the below R function I have used two function makeCacheMatrix and cache solve
##to find Inverse of a matrix in memory to avoid recalculation when the inverse is requested for the same matrix again 

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## calculates the inverse of the special matrix created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse
cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
  inv <- a$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
}
