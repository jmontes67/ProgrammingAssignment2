## Code to create a special "matrix" objetc to cache the inverse of a matrix x, 
## which is calculated with the function solve(x)

## This following function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
	  getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(getmatrix = getmatrix,setinverse = setinverse,
		 getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## function should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
	  inv <- makeCacheMatrix(x)$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- makeCacheMatrix(x)$getmatrix()
        inv <- solve(matrix)
        makeCacheMatrix(x)$setinverse(inv)
	  ## Return a matrix that is the inverse of 'x'
	  inv
}
