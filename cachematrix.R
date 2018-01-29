## The following two functions cache the inverse of a matrix to avoid computing the inverse repeatedly.


## The function makeCacheMatrix creates a matrix object that can cache its inverse. It returns a list containing the functions to set the matrix, get the matrix, set the inverse of the matrix (and cache it) and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inv <<- inverse
        getinverse <- function() x_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function computes the inverse of the matrix created with the function makeCacheMatrix. The function first checks if the inverse has already been calculated and in that case it retrieves the inverse from the cache without new computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinverse()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinverse(x_inv)
        x_inv
}
