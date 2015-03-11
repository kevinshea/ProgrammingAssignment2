## Functions to solve matrix for inverse and cache result

## function for caching value for inverse of a matrix
## returns a list of functions for: get, set, set_inverse, get_inverse
makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL #initialize my_inverse to null
  
  # set default values for matrix x and my_inverse
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  
  # function to return matrix x
  get <- function() x
  
  # function to use to solve inverse
  set_inverse <- function(solve) my_inverse <<- solve
  
  # function to return my_inverse
  get_inverse <- function() my_inverse
  
  # list of functions to use for obtaining cached solution
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Function to solve inverse of matrix
## takes list of functions as agrument
cacheSolve <- function(x, ...) {
  # gets cached value of inverse     
  my_inverse <- x$get_inverse()
  
  
  if(!is.null(my_inverse)) {
    # if inverse is already calculated, return that inverse
    message("getting cached data")
    return(my_inverse)
  }
  
  # get matrix data
  my_matrix <- x$get()
  # calculate inverse of matrix, my_matrix
  my_inverse <- solve(my_matrix)
  # set inverse, so we have it cached for later
  x$set_inverse(my_inverse)
  # return the inverse of my_matrix
  my_inverse
}
