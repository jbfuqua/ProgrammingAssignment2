## These functions compute the inverse of a given square matrix.  The first function calculates the inverse and caches
## the result, along with a copy of the matrix (for comparison with a later request to calculate the inversion -- 
## if the matrix is unchanged, return the cache, otherwise, return the re-calculated inversion)


## This function will return a list of funcitons that will:
##  Set the value of the vector
##  Return the value of the vector
##  Set the inverse of a square matrix
##  Return the matrix used to set the cached value (if set, NULL if not)
##  Return the cached inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {

  ## 'invx' is the cached inverse matrix
  ## 'refmat' is a copy of the matrix used to caculate the inverse - used to ensure no values have changed before
  ## returning the cached value
  
  invx <- NULL
  refmat <- NULL
  
  ## 'set' function creates the list of functions and sets the inverse result matrix to NULL
  
  set <- function(y) {
    x <<- y
    invx <<- NULL
    refmat <<- NULL
  }
  
  ## return the matrix
  
  get <- function () x
  
  ## calculate the inverse matrix, create a reference matrix to determine if x has changed
  
  setinv <- function(inverse) {
    refmat <<- x
    invx <<- inverse
  }
  
  ## return the inverse matrix
  
  getinv <- function() invx
  
  ## return reference matrix for comparison
  
  compmat <- function() refmat
  
  ## build the list to return
  
  list(set = set, get = get, setinv = setinv, getinv = getinv, compmat = compmat)
}


## This function calculates the inverse of a square matrix, first checking whether a cached result exists (checking
## that the input matrix values have not changed)

cacheSolve <- function(x, ...) {
 
  invmat <- x$getinv()
  matrix <- x$get()
  
  if(!is.null(invmat) && x$compmat() == matrix) {
      print('returning cached inverse')
      return (invmat)
      
  }
  
  ## otherwise, set the inverse and return
  else { 
    
    ## no error checking, per instructions -- assume input matrix inverse is solvable
    invmat <- solve(matrix,...)
    
    x$setinv(invmat)
    invmat
      
  }
}  
