## These functions implement an extension to the matrix object that is
## able to cache the inverse matrix

## Creates a special "matrix" which is a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the inverse matrix
# - get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          # initial value of inverse
  set <- function(y) { # function to set value
	  x <<- y
	  inv <<- NULL       # when setting value, remove previous inverse
  }
  get <- function() x  # function to get the value
  setinv <- function(inverse) inv <<- inverse  # function to set the inverse to a specified value
  getinv <- function() inv  # function to get the inverse
  list(set = set, get = get,  
       setinv = setinv,
       getinv = getinv)  # return a list of these four functions
}


## Calculates the inverse of the "matrix" created above, but only if
# not already cached.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  # store the cached inverse in inv
  if(!is.null(inv)) {  # if cached, just return it
    message("getting cached data")
    return(inv)
  } 
  # if not cached...
  data <- x$get()         # ... get the matrix
  inv <- solve(data, ...) #     compute the inverse
  x$setinv(inv)           #     set the inverse
  inv                     #     return it
}


# example code:
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
