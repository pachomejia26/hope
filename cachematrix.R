## My function create a list of functions that let us bring from 
## the cache memory the inverse of matrix which was calculeted in
##previous step

## this function stands for a square not singular matrix

makeCacheMatrix <- function(x = matrix()) 
{
  #x must be an square matrix
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}    



## this function actually calculates the inverse of the matrix
## to be used by MakeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m       
}
