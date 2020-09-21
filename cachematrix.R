

## There are two functions makeCacheMatrix and cacheSolve
##makeCacheMatrix consists in set,get,setInverse,getInverse
##cacheSolve is used to calculate inverse of matrices

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get , setInverse=setInverse, getInverse=getInverse)
}


  ## Function used to get the cache data

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("obteniendo cache data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv    
   ## Return a matrix that is the inverse of 'x'
}
