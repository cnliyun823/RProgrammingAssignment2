## My functions will create a special "matrix" that stores 
## the inverse of a matrix and cache's the inverse matrix

## This function checks whether the input matrix is square
is.square.matrix <- function(x=matrix()){
	  NRows <- dim(x)[1]
	  NCols <- dim(x)[2]
	  if (NRows == NCols) {
	  	  TRUE} else {
		  FALSE}
}

## This function creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
	  if (is.square.matrix(data)==TRUE){
		    inverse <- solve(data, ...)
}
	  else {
	          print("It is not a square matrix")
	  
}
                x$setinverse(inverse)
		    inverse
}