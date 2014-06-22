
##  PURPOSE OF THIS FILE & FUNCTIONS:
##
##  Matrix inversion is usually a costly computation. So, there may be some benefit to 
##  caching the inverse of a matrix rather than compute it repeatedly.  This file has 
##  a pair of functions that cache the inverse of a matrix, and look up this cached value, 
##  if the need arises in future (to compute the same inverse again) 
##
##  CONDITIONS  IF YOU WANT TO  TEST OR USE THESE FUNCTIONS:
##
##  According to the specifications given for this project, the input matrix can be 
##  assumed to be 'invertible'. See www.wikipedia.com for details on which matrices have an 'inverse'
##  because NOT all matrices have an inverse. So, these functions can not be used on "any" input.
##  Make sure the input matrix is 'invertible', to avoid error conditions.


## Function Name:  makeCacheMatrix
##
## Input:          an invertible  matrix.  (tests to make sure the input is invertible, even though 
##                 this is not required, according to given specifications).
##
## Details:        Creates a "special matrix" object, which can cache the inverse of the input given
##
## Output:         returns a list, containing the following functions. Note that these 
##                      functions implement the "special matrix" object, which can hold a cached result.
## 
##                      1. setMatrix    - save given matrix inside this "special matrix" object 
##                      2. getMatrix    - returns the matrix stored in this "special matrix" object
##                      3. setInverse   - store given matrix as the inverse value, inside this object
##                      4. getInverse   - returns the inverse matrix, stored inside this object
## 
makeCacheMatrix <- function(original_matrix = matrix()) {  # default value is an empty matrix.
        
        inverse_value <- NULL          # initialize the 'cache' variable
        setMatrix <- function(y) {     # store matrix 'y' inside this object and reset the 'cache'
                original_matrix   <<- y
                inverse_value     <<- NULL
        }
        
        # last statement/expression in an R function, is the return value of that function.
        getMatrix  <- function() original_matrix     

        setInverse <- function(calculated_inverse) {
                # note the use of <<- in this function, to use the lexical scoping rules
                # of R, to our advantage, and implement this "special matrix" object, as cache.
                inverse_value  <<- calculated_inverse   
        }

        # return the inverse_value, that is stored inside 'this' object.
        getInverse <- function() inverse_value

        # return value of makeCacheMatrix() is the list of functions, which are defined above.
        # Each function is a "named element" in this list, for convenience and clarity during use.
        list(set = setMatrix, get = getMatrix,  setInverse = setInverse,  getInverse = getInverse)
        
}       # end of makeCacheMatrix() 


## Function Name:  cacheSolve
##
## Input:          The "special matrix" object, that was created using makeCacheMatrix().
##
## Details:        computes the inverse matrix (if necessary). This inverse matrix result is stored 
##                 in the input given (the "special matrix" object called cacheMatrix).
##                 Thus, if we have to compute the inverse of same original matrix in future, 
##                 the computation need not be repeated, because the result is already available.
##                 
## Output:         Returns inverse of matrix stored in the "special matrix" object
##                 (called  cacheMatrix,  which is the input to this function)

cacheSolve <- function(cacheMatrix, ...) {
        
        inverse_result  <- cacheMatrix$getInverse()     # Check the "special matrix" object (cache)
        if(  !is.null(inverse_result)  ) {              # to see if we have a non-NULL inverse.
                message("getting cached data")          # 
                return(inverse_result)                  # If so, no need to solve for inverse again.
        }
        original_matrix  <- cacheMatrix$get()           # extract matrix for which we need inverse
        inverse_result   <- solve(original_matrix, ...) # compute the inverse, using R function.
        cacheMatrix$setInverse( inverse_result )        # store the inverse for future use
        return (inverse_result)                         # return the inverse matrix.
        
}       # end of cacheSolve()
