## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a square matrix using the matrix() function. The default is a 1x1 matrix with each element = NA

makeCacheMatrix <- function() {
  
  # this outer function defines the 4 functions we need: setmatrix/getmatrix and setinverse/getinverse. These 4
  # are returned as elements of a list in the final step of this function
  # How to use this function:
  # a <- makeCacheMatrix()
  # call each of the 4 functions defined below by prepending a$ to each function name
  # e.g. a$setmatrix(1:4,2,2) will create a 2x2 matrix containg 1,2,3,4. Note warning below about singular matrices.
  # a$getmatrix() wil display the last matrix created
  # a$setinverse() will invert the last matrix created
  # a$getinverted() will display the last inverted matrix
  
  # function #1: setmatrix() initializes a matrix with the specified matrix size and contents
  
  # NB: A matrix must be square before it can be inverted, but SOME SQUARE MATRICES CANNOT BE INVERTED
  # (see http://www.mathsisfun.com/algebra/matrix-inverse.html). A matrix with no inverse is called
  # a "singular" matrix. You will get an error something like this if you choose one of these singular matrices: 
  # solve(m) Error in solve.default(m)
  # Lapack routine dgesv: system is exactly singular: U[3,3]=0
  # If that happens, choose another matrix.
  
  setmatrix <- function(data = NA, nrow = 1, ncol = 1) 
  { m <<- matrix(data, nrow, ncol)
  }
  
  # function #2: getmatrix() retrieves the result of the last setmatrix() command
  getmatrix<- function() m
  
  # function #3: invertmatrix() inverts matrix m (the result of the last setmatrix() call) and caches it
  invertmatrix <- function() {
    i <<- solve(m)
  }
  # function #4: getinverted() shows the inverted matrix produced by the last call to invertmatrix()
  getinverted <- function() i
  
  
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, invertmatrix = invertmatrix, getinverted = getinverted)
  
}


## Write a short comment describing this function
# This function calculates the inverse of the matrix x passed as its argument

cacheSolve <- function(x, ...) {
  ## Calculates an inverted matrix. The original matrix has already been created using the setmatrix() function in the 
  ## makeCacheMatrix function above (see instructions there to learn how to create a matrix). This function (cacheSolve)
  ## will check to see if the inverted function has already been created. If it has, it won't perform the
  ## operation again, but instead will report the existing inverted matrix. If no inverted matrix exists,
  ## then cacheSolve() will call invertmatrix()
  
  print("Current matrix:")
  m
  i <- x$getinverted()
  # if i is not null (i.e. it contains some value), then there's no need to invert
  # the matrix - it's already been done - so just exit from the function
  if (!is.null(i)) {
      message("Getting cached inverted matrix")
      return(i)
  }
  # otherwise, get the current matrix
  m <<- x$getmatrix()
  # and invert it
  i<<- invertmatrix()
  
}