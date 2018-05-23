## Pair of functions that calculate and cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## Contains methods to get and set matrix value, as well as get and set inverse

makeCacheMatrix <- function(x = matrix()) {
	
    #Initialize NULL inverse matrix.  Allows checking for and caching inverse.
    inv <- NULL
    
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	#inverse functions
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	
	list(set = set, get = get,
	     setinv = setinv, getinv = getinv)
}


## Checks for a null inverse matrix, calculates, and caches

cacheSolve <- function(x, ...) {
    
    #assign current stored inverse    
    inv <- x$getinv()
    
    #check if inverse is null, calculate if so
    if (is.null(inv)){
        
        message("Calculating and caching inverse.")
        inv <- solve(x$get())
        x$setinv(inv)
        return(inv)
    }
    #if not null, return calculated inverse
    else
    {
        message("Returning cached inverse.")
        return(inv)
    }
    
}
