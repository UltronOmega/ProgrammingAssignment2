##cachematrix.R is a function that returns the inverse of matrix presuming all input matrix are invertible##
##The upper function makeCacheMatrix can cache the inverse of matrix if it already exists##
##if we add new matrix, it will recalculate the inverse and cache it##

makeCacheMatrix<-function(x=matrix()){
  m<<-NULL
  set<<-function(y=matrix()){
    x<<-y
    m<<-NULL    
  }
  get<-function() {x}
  setinverse<-function(inverseval){m<<-inverseval}
  getinverse<-function() {m}
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
##cacheSolve is a function that returns the inverse of matrix presuming all input matrix are invertible##
##cacheSolve first checks if the inverse value of matrix is already stored in getinverse of cachematrix, 
##if inverse matrix value stored it returns that from cache otherwise it recalculates the inverse and store it in setinverse of makeCacheMatrix 

cacheSolve<-function(x,...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
