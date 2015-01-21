## cacheSolve is called with a "matrix" object as input.
## Uses the "matrix" object to obtain a matrix value.
## If the matrix value already has an inverse, it is retrieved from the "matrix" cache.
## Otherwise, the inverse of the matrix is computed and store in the "matrix" cache.
## Thus, the inverse is only computed when the "matrix" cache contains no inverse.

## Return a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
	
	inv <- x$getinverse() # Use the getinverse method of the matrix object 
	
	if(!is.null(inv)) { # If the inverse obtained above is not null...
		message("Getting cached inverse.") 	
		
	
	}
	else { # otherwise...
		
		inv <- solve(x$get(), ...) # ... compute the inverse
		x$setinverse(inv)          # ... and reset it in the cache
		
	}
		return(inv) # ... return the inverse, whether from the cache or computed
}
