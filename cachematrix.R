## Below are two functions that are used to create a special object 
## that stores a inverse of a matrix in the cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    # set inverse as NULL
    inverse <- NULL
    
    # set method
    set <- function(mtx) {
        x <<- mtx;
        inverse <<- NULL;
    }
    
    # get cashe matrix : retrive value x
    get <- function() x
    
    # set inverse method : set inverse value in cache
    setinverse <- function(invmtx) inverse <<- invmtx
    
    # get inverse method : retrive inversed value from cache
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinvere)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    xinverse <- x$getmean()
    
    # if there is inverse value in the cache, return the veale
    if(!is.null(xinverse)) {  
        message("Getting cached data")
        return(xinverse)
    }
    
    # retrive value from the cache
    data <- x$get()
    
    # Calculate inverse of matrix
    xinverse <- solve(data, ...)

    # set inversed value in the cache
    x$setinverse(xinverse)
    
    # returen inversed value
    xinverse    
}

