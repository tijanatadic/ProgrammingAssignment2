## Put comments here that give an overall description of what your
## functions do

## This function creates a special object matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i<<-NULL  #sets a default value to an inverted matrix, NULL if cacheSolve has not been called
        set<-function(y){
                x<<-y      #store the unput matrix
                i<<-NULL
        }
        get<-function() x  #returns cached value
        setinv<-function(solve) i<<-solve
        getinv<-function() i
        list(set=set,get=get,setinv=setinv,getinv=getinv) #create a list which stores 4 functions
        
}

## Write a short comment describing this function

cacheSolve <- function(x = matrix()) {
        i<-x$getinv()
        if(!is.null(i)){
                message("Getting cached matrix")
                return(i)
        }
        
        data<-x$get()
        inv<-solve(data)
        x$setinv(inv)
        inv
}
