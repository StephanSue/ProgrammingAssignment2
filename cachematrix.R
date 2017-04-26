## The following functions are able to store a matrix and cache's the
## inverse of the matrix that it can be reused without recalculation

# This function returns a list of functions, stores a matrix and caches the inverse 
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL        
        set <- function(y){                
                x <<- y                
                inv <<- NULL        
        }        
        get <- function() x        
        setinv <- function(inverse) inv <<- inverse        
        getinv <- function() inv        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



# This function calculate the inverse of a matrix created with makeCaheMatrix.
# If the result is in the cache already it calls the cache, otherwise it is calculated.
cacheSolve <- function(x, ...){        
        inv <- x$getinv()        
        if (!is.null(inv)){                
                message("getting cached data")                
                return(inv)        
        }        
        data <- x$get()        
        inv <- solve(data, ...)       
        x$setinv(inv)        
        inv
}
