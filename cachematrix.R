## The makeCacheMatrix and cacheSolve functions check to see if 
## the inverse of a matrix has already been calculated, and if yes
## the calculated inverse will be retrieved from the cache.
## If it hasn't been calculated yet, it will be calculated.

## The makeCacheMatrix function will return an object of type
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y) {
		x <<- y
		matinv <<- NULL
	}
	get <- function() x
	setinverse <- function() matinv <<- solve(x)
	getinverse <- function() matinv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function will take an object, and attempts to 'get'
## the inverse of the matrix.  If result it NULL, then the function
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinverse
        if(!is.null(matinv)) {
        	message("getting cached inverse data")
        	return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setInverse(matinv)
}
