## These is a pair of functions that cache the inverse of a matrix.

## A function that creats a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set<-function(y){
        x<<-y
        inverse<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) inverse<<-inverse
    getinverse<-function() inverse
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse<-x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inverse<-solve(data, ...)
    x$setinverse(inverse)
    inverse  ## Return a matrix that is the inverse of 'x'
}


