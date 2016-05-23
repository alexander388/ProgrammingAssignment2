#Matrix Inversion is typically a time consuming computation
#Catching the inverse of a matrix is more beneficial than repeatatively compute it.
#The functions below are written for the purpose of caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
	
	#cache is set to NULL initially as there is nothing cached.
	cache <- NULL
	
	#Storing a matrix.
	set <- function(y) {
		x <<- y
		cache <<- NULL
	}

	#returning the matrix.
	get <- function() {
		x
	}

	#cache the given argument.
	cacheInverse <- function(inverse){ 
		cache <<- inverse
	}
	
	#show cached value.
	getInverse <- function (){
		cache
	}

	#return list.
	list(set = set,
		get = get,
		cacheInverse = cacheInverse,
		getInverse = getInverse)
}


#This function serves to calculate the inverse of a special matrix through makeCacheMaterix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	cache <- x$getInverse()
	if (!is.null(cache)){
		message("getting cached data")
		return(cache)

	}

	mat <- x$get()
	
	cache <- solve(mat)

	x$cacheInverse(cache)

	cache
}
