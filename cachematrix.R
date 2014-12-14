## The makeCacheMatrix function creates a special matrix object which caches 
## its inverse matrix. 
## The cacheSolve function accesses the object created with makeCacheMatrix.
## If its inverse matrix was previously catched then only returns it. If not, 
## computes the inverse matrix and caches it in the special matrix object.

## -----------------------------------------------------------------------------

## makeCacheMatrix(x)
##
## This function creates a special "matrix" object. 
## The object stores two values: 
## 	 the original matrix (in the object "x"), and 
## 	 the inverse matrix (in the object "inv", initially set to NULL). 
##
## If no argument is passed, creates blank matrix (1x1 containing "NA").
##
## The special matrix object has four methods
## 	 set   : re-sets the matrix to new values and sets the inverse matrix "inv" 
## 	         to NULL,
## 	 get   : gets the values in the original matrix,
## 	 setinv: stores the inverse of the matrix in the object "inv", and
## 	 getinv: gets the inverse matrix "inv".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(n) { 
  # Re-setting the matrix to new values and setting "inv" to NULL
    x <<- n
    inv <<- NULL
  }
  
  get <- function() {
  # Getting the original matrix "x"
    x
  }
  
  setinv <- function(inverse) {
  # Saving the inverse of the matrix in the object "inv"
    inv <<- inverse
  }
  
  getinv <- function() {
  # Getting the inverse matrix ("inv")
    inv
  }
  
  # List of methods:
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## -----------------------------------------------------------------------------

## cacheSolve(x,...)
##
## This function computes (or fetches) the inverse of the special "matrix" 
## created by makeCacheMatrix.
## 
## The function gets the inverse matrix stored in the special "matrix" object
## calling the method getinv(). 
##
## If gets NULL, computes the inverse matrix and stores it in the special 
## "matrix" object, using the method setinv(), 
##
## If gets the inverse matrix cached in the special "matrix" object, returns the
## fetched inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()                         # Fetching the inverse matrix
  if (!is.null(m)) {                      # If was cached previously
    message("Getting inverse matrix.")    # displays a message, and
    return(m)                             # returns the inverse matrix.
  } else {                                # If the inverse matrix wasn't cached
    matr <- x$get()                       # gets the original matrix
    inv <- solve(matr)                    # computes the inverse matrix
    x$setinv(inv)                         # caches the inverse matrix, and
    inv                                   # returns the inverse matrix.
  }
}
