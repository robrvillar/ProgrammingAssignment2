## Matrix inversion is usually a costly computation.  
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## First it sets and gets the value of the matrix.
## Then it sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        mxinv <- NULL
        set <- function(y) {
                x <<- y
                mxinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mxinv <<- inverse
        getinverse <- function() mxinv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.
## If not, it computes the inverse and sets the value in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mxinv <- x$getinverse()
        if(!is.null(mxinv)) {
                message("getting cached data.")
                return(mxinv)
        }
        data <- x$get()
        mxinv <- solve(data)
        x$setinverse(mxinv)
        mxinv
}
