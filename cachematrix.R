## Two functions that cache the inverse of a matrix

#This function create a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x<<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse  = getInverse )
  
}

## This function computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  if(!is.null(im)){
    message("Getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setInverse(im)
  im
}
