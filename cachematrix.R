## Creates a special matrix object which can cache its inverse
## This will enable speedup of repeated matrix inversion by accessing its cache

##  makeCacheMatrix implements 3 functions 
##  get (returns the special matrix object)
##  setmatrix (caches the matrix in calling environment)
##  getmatrix (reads the matrix from the cache)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Computes the inverse of an invertable matrix. If the invertable matrix
## is already stored in cache, returns that (saves computation time)

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  
  # if matrix already exists in cache retrieve and return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calculate inverse of matrix and store in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
