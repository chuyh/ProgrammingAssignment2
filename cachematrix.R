## This functiones are for the Assignment2. As per instructions I'm creating a first function
## that is in charge of manipulating the values of the matrix.

## makeCacheMatrix gets and sets the values
## of the matrix in which we perform the Inverse. the get and set functions are in charge
## of retrieving and mutating the values of the original matrix. The setinverse and getinverse
## are the ones which 

makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL
  set <- function(new_matrix)
  {
    x <<- new_matrix ## adding the new matrix variable to the global environment
    matrix_inverse <<- NULL ## setting the global inversed matrix to NULL
  }
  get <- function() x ## retrieve the value of the latest version of original matrix
  
  setinverse <- function(inverse) matrix_inverse <<- inverse ## adding the new inversed matrix variable to the global environment
  getinverse <- function() matrix_inverse ## retrieve the value of the latest version of inversed matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## creating the list of functions
}


## The cache solve function is in charge of checking the existence of the matrix in cache
## in case it exists, the matrix is retrieved. In case it does not exist then the inverse
## is created and cached

cacheSolve <- function(x, ...) {

  matrix_inverse <- x$getinverse() ## getting the matrix from the global environment
  if(!is.null(matrix_inverse)) { ## checking if the matrix is NOT NULL
    message("getting cached data") ## Message while getting the cached matrix
    return(matrix_inverse) ## returning the cached matrix
  }
  data <- x$get() ## retrieving the original matrix from the global variable
  matrix_inverse <- solve(data) ## calculating the inverse of the original matrix
  x$setinverse(matrix_inverse) ## setting the global variable for the inversed matrix
  x$getinverse ## getting the global variable of the inverse matrix
}
