## Put comments here that give an overall description of what your
## functions do
#Two functions are created: makeCacheMatrix and cacheSolve
#Creating makeCacheMatrix function: creates a matrix that can cache its' inverse

makeCacheMatrix<- function(x= matrix()){
  inv<-NULL                              # initializing inverse as a Null
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x                     # function to get matrix X
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv            # function to obtain the inverse of matrix X
  list(set=set, get=get, 
       setInverse=setInverse,
       getInverse=getInverse)
}

#Creating CacheSolve function: computes the inverse of the special matrix created earlier
cacheSolve<-function(x, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){                   # Confirming whether inverse is Null
    message("getting cached data")
    return(inv)                        # calls inverse value
  }
  data<-x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv                                  # Returns a matrix that is the inverse of matrix X
}
