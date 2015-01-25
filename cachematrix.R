## This code creates 2 functions that cache the inverse of matrix rather than
## computing it repeatedly.

## makeCacheMatrix creates a special object that stores a matrix. A list is
## created which contains a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the value
## of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## creates an empty matrix as a placeholder to store the value in later
  set <- function(y) { ## creates "set" function with variable "y"
    x <<- y            ## x is assigned to the value of y in the 
                       ## environment of the containing "set" function
    inv <<- NULL       ## assigns inv matrix to NULL in the containing function
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will return the inverse of a matrix "a". It will first check if the 
## matrix has been solved earlier in the makeCacheMatrix function. If yes, it will
## return the value of what has been computed. If not, it will compute the inverse

cacheSolve <- function(a, ...) {
    inv <- a$getinverse() ## assign inv to the inverse of matrix a
    if(!is.null(inv)) { ## check if the inverse has been calculated before. if
                        ## yes, return message and the inverse
      message("getting cached data")
      return(inv)
    }
    data <- a$get()     ## if inverse has not been calculated before, solve for it
    inv <- solve(data, ...)
    a$setinverse(inv)
    inv
}

######## TESTING OF CODE ########

# > x <- matrix(1:4, 2, 2)
# > a <- makeCacheMatrix(x)
# > cacheSolve(a) ## run cacheSolve for the first time
# > cacheSolve(a)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a) ## run cacheSolve for the second time
# getting cached data ## message appears to show that inverse is already stored
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5