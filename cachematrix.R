## The following 2 functions are meant to return the inverse of a matrix
## Since computing an inverse is a costly computation, we will create a function
## that can cache the inverse of a matrix. A new computation will only be ddone if a matrix  has changed.


## This function creates a special "matrix" object to cache its inverse.
## input to this function is a square invertible matrix
## output of this function is a list consisting of functions that:
## set the matrix, get the matrix, set the inverse and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse 
  
  getinverse <- function() inv 
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the input matrix has not changed and the inverse has already been calculated for the matrix,
## this function will retrieve the inverse from the cache
## else it will calculate the inverse for the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()        ##fetching the value of inverse calculated 
  
  if(!is.null(inv)){
    message("Inverse calculated already, getting inverse of the matrix from cached data")
    return(inv)
  }
  
  message("Matrix is new, calculating inverse")
  data <- x$get()             ##getting the matrix
  inv <- solve(data)          ## calculating the inverse of the matrix
  x$setinverse(inv)           ## setting the value of inverse 
  inv                         ## returning the calculated inverse
}
