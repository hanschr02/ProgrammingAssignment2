## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  ## Purpose: creates a special "matrix" object that can cache its inverse.
  ## input'x': matrix
  
  # define function to set a matrix (store matrix in cache)
  m <- NULL
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  # define function to get a matrix from cache
  get_matrix <- function() x
  
  # define function to store the inverse matrix in cache
  set_inv_matrix <- function(solve) m <<- solve
  # define function to get the inverse matrix
  get_inv_matrix <- function() m
  # store list of function and return them
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}


cacheSolve <- function(x, ...) {
  ## Purpose: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve retrieves the inverse from the cache.
  ## input'x': list of 4 functions
  
  # try to get the inverse matrix from cache
  m <- x$get_inv_matrix()
  # check, whether inverse matrix could be retrieved from cache
  if(!is.null(m)) {
    # if, yes, print out message and return inverse matrix
    message("getting cached inverse matrix")
    return(m)
  }
  
  # inverse matrix couldn't be rerieved from cache

  # get the original matrix
  data <- x$get_matrix()
  # calculate the inverse matrix
  m <- solve(data, ...)
  #write result back to cache
  x$set_inv_matrix(m)
  # return inverse matrix
  m
  
}
