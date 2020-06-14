#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
#The functions below cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		#set the value of the matrix
		set <- function(y) { 
			x <<- y
			m <<- NULL
		}
		#get the value of the matrix
		get <- function() x
		#set the value of the inverse
		setinverse <- function(inverse) m <<- inverse
		#get the value of the inverse
		getinverse <- function() m
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)
}


#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
