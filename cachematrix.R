## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix returns a list whose elements are four functions. Function get gets the 
## matrix x. Function set sets the matrix x, and when also updates value of invx to be NULL (with superassignment operator <<- ) so that inverse is calculated again when runing function cacheSolve.
## Function getInv and setInv return and set, respectively, the inverse of x. 

makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y){
    x<<-y
    invx<-NULL
  } 
  get<- function() x
  setInv<- function(inv) invx<<-inv
  getInv<- function () invx
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## First note that we assume that all matrices passed to the function cacheSolve are invertible, thereby square and of full rank.
## Function first checks if inverse of x has already been calculated. If it hasn't it calculates it. If it has then it returns already calculated inverse of x.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx<-x$getInv()
  if(!is.null(invx)){
    message("Getting cached inverse")
    return(invx)
  } else{
    matrix<-x$get()
    invx<-solve(matrix)
    x$setInv(invx)
    invx
  }
}
