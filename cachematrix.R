## Caching the inverse of a matrix may be beneficial since we can always retrieve it wihtout
## having the function to calculate it repeatedly and reduce computational cost.
## The functions will be creating a special 'matrix' that can cache its inverse and finally 
#computing inverse of the matrix returned by earlier function.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setdata <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getdata <- function() x
  setInv <- function(matInv) inv <<- matInv
  getInv <- function() inv
  list(set = setdata,
       get = getdata,
       setInverse = setInv,
       getInverse = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  dataInv <- x$get()
  inv <- solve(dataInv)
  x$setInverse(inv)
  inv      
}
  
  
  
