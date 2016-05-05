##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly 
##Here is a pair of functions that cache the inverse of a matrix.
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##   has not changed), then the cachesolve should retrieve the inverse from the cache.

##This functions assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {

        ## This function creates a special "matrix" object that can cache its inverse.
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##   cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##   has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        print(x)
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}