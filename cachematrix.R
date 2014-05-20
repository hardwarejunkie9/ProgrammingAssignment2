## This function establishes a "class" of matrix object.
##   CacheMatrix stores not only its original values, but also its 
##	inverse.

makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
	# Set establishes the matrix values, and NULLS m, the return 
	# variable for the inverse.
   set <- function(y) {
        x <<- y
        m <<- NULL
        }
	# Get returns the matrix values
   get <- function() x
	# setsolve() allows a function to pass the inverse to m.
   setsolve <- function(solve) m <<- solve
	# getsolve() returns the current stored solution from cache
   getsolve <- function() m
   list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve() takes x, a cacheMatrix() object, and checks if an
## inverse is currently stored

cacheSolve <- function(x, ...) {
      ## Return the stored inverse of 'x'
	 m <- x$getsolve()
	##if x$getsolve() contains data, return that
  	 if(!is.null(m)) {
      	message("getting cached data")
      	return(m)
     	 }

	## If there is no cache, pull the existing matrix into 'data'
	data <- x$get()
	## Find the inverse
      m <- solve(data, ...)
	## Store the inverse in x's cache
      x$setsolve(m)
	## Return the inverse.
      m

}

}