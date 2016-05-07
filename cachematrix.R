## The following functions, makeCacheMatrix and cacheSolve, allow one to store
## the computed inverse of a matrix for repeated use.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It creates a list containing a function to:
### set the value of the matrix
### get the value of the matrix
### set the value of the invers of the matrix
### get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function () x
        
        setinverse <- function (inverse) inv <<- inverse
        
        getinverse <- function () inv
        
        list (set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix is unchanged), then cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse ()
        
        if (!is.null (inv)) {
                message ("getting cached data")
                return (inv)
        }
        
        data <- x$get ()
        
        inv <- solve (data)
        
        x$setinverse (inv)
        
        inv
}
