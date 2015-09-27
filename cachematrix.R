## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get <-function() x
  setinv<-function(inverseMatrix) inv<<-inverseMatrix
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  message("computing from scratch")
  i<-solve(data,...)
  x$setinv(i)
  i
}


 ## just add a small function to test the previous functions
 ## source this code and then invoke test() in the R console
test <-function(){
 
  x<-makeCacheMatrix(x=matrix(data=c(c(1,2,3),c(34,22,3),c(8,3,3)),3,3))
  
  ##here computes
  i<-cacheSolve(x)
  
  #here gets from cache
  i<-cacheSolve(x)
  
  
}


