## Caching the inverse of a matrix


## This function creates a special matrix that can cache its inverse

library(matlib)
makecachematrix <- function(x=matrix(data)){
  m<-NULL
  set<-function(y){
    x<<-y
    m<-NULL
  }
  get<-function()x
  setinv<-function(inv) m<<-inv(x)
  getinv<-function()m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function calculates the inverse of a matrix returned by Makecachematrix above

cachesolve <-function(x,...){
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data1<-x$get()
  m<-inv(data1,...)
  x$setinv(m)
  m
}
