# The first function will create a list containing a function to set the matrix, get the matrix, set the inverse of the martix, and get the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
			m <- NULL
			set <- function (y) {
				x <<- y
				m <<- NULL
			}
			get <- function() x
			setinverse <- function(inverse) m <<- inverse
			getinverse <- function() m
			list( set = set, 
	     			get = get, 
           			setinverse = setinverse, 
           			getinverse = getinverse)
}

# The second function first checks to see if the inverse of the matrix has been solved and cached.  If so, it returns the cached matrix.  If not, it then solves the matrix.

cacheSolve <- function (x, ...) {
		m <- x$getinverse()
		if(!is.null(m)) {
				message("getting cached data")
    				return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}