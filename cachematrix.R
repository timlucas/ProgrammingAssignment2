## These functions allow you to solve for the inverse of matrices in a way
## that caches the inverse for a given matrix once solved for the first time.
## Subsequent calls to get the inverse will read from the cache, saving time.

## makeCacheMatrix creates a special matrix, which is really a list containing 
## functions to get/set the value of the matrix, and get/set the value of its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve solves a matrix for its inverse. If the inverse has already 
## been solved, the inverse will be read from cache.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m

}
