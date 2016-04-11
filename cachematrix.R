## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i_ <- NULL #closure
	set <- function(n){
		x <<- n
		i_ <<- NULL
	}
	get <- function() x #return matrix
	set_inverse <- function(inv) i_ <<- inv  
	get_inverse <- function() i_
	# return
	list(set=set,
		get=get,
		getInv=get_inverse,
		setInv=set_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	# Some error checking
	if (!det(x$get())){
		# Stop execution
		stop('The input argument for cacheSolve does not have an inverse')
		# Replacement for cryptic
		# Lapack routine dgesv: system is exactly singular: U[n,n] = 0
	}

	# Try restoring cache
	i_ <- x$getInv()
	if (!is.null(i_)){ 
		message("returning cached inverse")
		return(i_)
	}

	# get inverse and store to cache
	data <- x$get()
	i_ <- solve(data, ...)
	x$setInv(i_)
	i_

        ## Return a matrix that is the inverse of 'x'
}

# Some tests 
testCache <- function(squareSze=4){
	b <- matrix(runif(squareSze**2, -5.0, 5.0), nrow=squareSze, ncol=squareSze)	
	a <- makeCacheMatrix(b)
	message('First Run store inverse to cache')
	print(cacheSolve(a))
	message('Test Get Inverse from cache')
	print(cacheSolve(a))
	# Test error handling
	b <- matrix(1:16, nrow=4, ncol=4)
	a <- makeCacheMatrix(b)
	print(cacheSolve(a))
}
