## Create a special "matrix" object that can cache its inverse

## This function accepts a matrix as input and creates "matrix" object.
## The object has 4 methods: set, get, setinverse, getinverse.
## If set is invoked with the same matrix twice in succession, there is no action.
## Otherwise, the new matrix is remembered and the inverse is set to null.
## The cacheSolve function will either compute the inverse and set it in the "matrix"
## object with which it was invoked, or will return the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
	imat <- NULL # Set cached inverse, imat, to NULL

	if(det(x)==0) { # Check to make sure matrix is invertible
	  message("The matrix has no inverse.")
	  return()
	}
	set<- function(y) { # Method to (re)set the cached matrix.
		if(identical(x,y)) { return() } # If matrix is same as previous, return.
		else { # Otherwise...
			x <<- y  # Replace the cached matrix with new one.
			imat <<- NULL  # Set the cached inverse to NULL.
		}
	}
	
	get <- function() x # Return the cached matrix
  	setinverse <- function(inverse) imat <<- inverse # Reset the cached inverse.
	getinverse <- function() imat   # Return the cached inverse

## Specify the methods used in the cacheMatrix object.	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
		
}



