## Put comments here that give an overall description of what your
## functions do

## Comments by Eduardo Ramirez: 
##      1. Hereafter we cache computations that are normally expected to be time consuming, as is matrix inversion.
##      2. In such way that when we call for these results, they are already available without needing to be computed again. 

## Write a short comment describing the first function:
##      1. Our first function is utilised to cache a matrix. 
##      2. One assumption is that the matrix is invertible. Therefore "inv = NULL"
##      3. We set a value for the matrix using the "set" function.
##      4. Our double arrow assignment operator "<<-" helps us modify variables in parent levels.
##      5. (Whereas the single arrow assignment operator "<-" always works on the current level). 
##      6. Beyond our previously established "set" function, we get the value of the matrix assigning it to the variable "get".
##      7. We set the value of the inverse through a function assinged to the "setInverse" variable.
##      8. And --similarly-- we get the value of the inverse through a function assinged to the "getInverse" variable.
##      9. Finally, we create a list that contains all of our elements. 
##      10. Usage goes as follows: 
  

makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing the second function
##      1. Our "cacheSolve" function is utilised to compute the inverse of our cached matrix 
##      2. (Said cached matrix was previously created by our "makeCacheMatrix" function).
##      2. In case the inverse of our matrix has already been calculated and cached withour change, 
##      this value will be displayed right away rather than being re-calculated. 
##      3. On the otherhand --if there was change-- the user indicates this through the "set" function,
##      and a new inverse matrix calculation is made.  


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


##      Thank you for reading. 





