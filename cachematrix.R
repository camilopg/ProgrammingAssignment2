##1. makeCacheMatrix: This function 
##creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
        e<-NULL
        set<-function(y){
                x<<-y
                e<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse)e<<-inverse
        getInverse<-function()e
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

##2. cacheSolve: This function computes the inverse of the special matrix
##returned by makeCacheMatrix. If the inverse has already been calculated
##(and the matrix has not changed),then the cachesolve should
##retrieve the inverse from the cache.

cacheSolve<-function(x,...){
        e<-x$getInverse()
        if(!is.null(e)){
                message("searching cached data")
                return(e)
        }
        data<-x$get()
        e<-solve(data,...)
        x$setInverse(e)
        e
}

