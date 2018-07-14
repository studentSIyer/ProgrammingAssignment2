## The first function, `makeCacheMatrix` creates a special "Matrix", which is
## really a list containing a function to set, get the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
		setInverse <- function(solve) m <<- solve
            getInverse <- function() m
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## The following function calculates the nverse of the special "Matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setInverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getInverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data,...)
            x$setInverse(m)
            m
}


