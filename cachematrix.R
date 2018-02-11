## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve are functions for computing inversible
## matricies and then caching them for later use. 

## Write a short comment describing this function
## makeCacheMatrix is a function that creates an inversed matrix and 
## caches it.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Write a short comment describing this function
## cacheSolve is function that can either compute the inverse matrix 
## returned by makeCacheMatrix or if the inverse has been calculated with
## an unchanged matrix, retrieve the inverse from cache. 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
