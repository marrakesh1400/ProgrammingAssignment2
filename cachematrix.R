## This R script creates two functions, makeCacheMatrix() 
# and cacheSolve(). makeCacheMatrix() creates a cached 
# matrix and functions to calculate the matrix inverse on
# it. cacheSolve() uses the functions listed in makeCacheMatrix() 
# to calculate the matrix inverse.
# The syntax is:
# j = makeCacheMatrix(x)
# cacheSolve(j)
# where x is an invertible matrix
#
# Here is an example:
# > A = rbind(c(1,-.05),c(-0.05,1))
# > j = makeCacheMatrix(A)
# > cacheSolve(j)
# [,1]       [,2]
# [1,] 1.00250627 0.05012531
# [2,] 0.05012531 1.00250627
################################################################

## This function creates a list of four functions:
#  set() - set matrix  
#  get() - get matrix
#  setinv() - set value of matrix inverse
#  getinv() - get value of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # set value of matrix inverse
    setinv <- function(solve) m <<- solve
    # get value of matrix inverse
    getinv <- function() m
    # create list of those functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve uses the functions created by makeCacheMatrix()
# to return the inverse of the cached matrix created with 
# makeCacheMatrix(). 
cacheSolve <- function(x, ...) {
    # get cached matrix
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Return inverse of 'x'
    m <- solve(data, ...)
    x$setinv(m)
    m
}
