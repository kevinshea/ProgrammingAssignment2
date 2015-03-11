## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  
  get <- function() x
  set_inverse <- function(inverse) my_inverse <<- inverse
  get_inverse <- function() my_inverse
  
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my_inverse <- x$get_inverse()
  
  if(!is.null(my_inverse)) {
    message("getting cached data")
    return(my_inverse)
  }
  
  my_matrix <- x$get()
  my_inverse <- solve(my_matrix)
  x$set_inverse(my_inverse)
  my_inverse
}
