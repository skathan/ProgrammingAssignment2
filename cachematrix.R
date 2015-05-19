## Functions to save, cache, and provide the inverse of a matrix -- by NJK

## Function to get, set, and cache the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-NULL
  }
  
  get <- function(){
    x
  } 
  
  setmatrix <- function(solve){
    m <<- solve
  } 
  
  getmatrix <- function(){
    m
  } 
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## function to check for cached data and provide inverse of matrix

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
