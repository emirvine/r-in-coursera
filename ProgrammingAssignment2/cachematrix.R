## Creating a matrix object that can cache its inverse without looping
## to reduce computational costs.
## This function sets the value for the matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
	
	get <- function() x
	setmatrix <- function(inv) m <<- inv
	getmatrix <- function() m
	list(set = set, get = get,
	setmatrix = setmatrix,
	getmatrix = getmatrix)
}

## Helper function to compute the inverse of the matrix created above.
## It first checks if the inverse has already been computed, if so it 
## returns that value, otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	# checks if cacheSolve has already been executed
	if(!is.null(m)) {
		message("Getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix)
	x$setmatrix(m)
	m # returns the inverse
}