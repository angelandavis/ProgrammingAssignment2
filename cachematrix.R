## Assignment 2 - R Programming Notes
##      makeCacheMatrix: This function creates a special "matrix" object that 
##              can cache its inverse.
##      cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. 
##      assign2: This function used by testing of the cache function. 

##      If the inverse has already been calculated (and the matrix has not 
##              changed), then the cachesolve should retrieve the inverse from
##              the cache.

##      Computing the inverse of a square matrix can be done with the solve 
##              function in R. For example, if X is a square invertible 
##              matrix, then solve(X) returns its inverse.

##      For this assignment, assume that the matrix supplied is always 
##              invertible.

makeCacheMatrix <- function(x=matrix()) {
        x <<- y
        inv <<- NULL
        cachex<<-x
        
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        get<- function()x
        setinv <- function(inverse) inv<<-inverse
        getinv <- function() inv
        list(set=set,get=get,
             setinv=setinv,getinv=getinv
        )
        
}

cacheSolve <- function(x,...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

assign2 = function(mat) {
        ## create the cache Matrix
        temp=makeCacheMatrix(mat)
        
        ## get the inverse and capture duration
        start.time=Sys.time()
        cacheSolve(temp)
        dur=Sys.time() - start.time
        print(dur)
        
        ## call for inverse again, should see cache, and capture duration to get cache
        start.time=Sys.time()
        cacheSolve(temp)
        dur=Sys.time() - start.time
        print(dur)
        
}
